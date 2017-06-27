-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.Helpers
  ( decimalsToDouble
  , double
  , integer
  , multiply
  , divide
  , notOkForAnyTime
  , numberBetween
  , numberWith
  , oneOf
  , parseDouble
  , parseInt
  , parseInteger
  , withGrain
  , withMultipliable
  , parseDecimal
  ) where

import Data.Maybe
import Data.Text (Text)
import Prelude
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Numeral.Types
import Duckling.Types hiding (Entity(value))

zeroT :: Text
zeroT = Text.singleton '0'

dot :: Text
dot = Text.singleton '.'

comma :: Text
comma = Text.singleton ','

parseInt :: Text -> Maybe Int
parseInt = (fromIntegral <$>) . parseInteger

parseInteger :: Text -> Maybe Integer
parseInteger =
  either (const Nothing) Just . Atto.parseOnly (Atto.signed Atto.decimal)

-- | Add leading 0 when leading . for double parsing to succeed
parseDouble :: Text -> Maybe Double
parseDouble s
  | Text.head s == '.' = go $ Text.append zeroT s
  | otherwise = go s
  where go = either (const Nothing) Just . Atto.parseOnly Atto.double

-- | 77 -> .77
-- | Find the first power of ten larger that the actual number
-- | Use it to divide x
decimalsToDouble :: Double -> Double
decimalsToDouble x =
  let xs = filter (\y -> x - y < 0)
         . take 10
         . iterate (*10) $ 1 in
    case xs of
      [] -> 0
      (multiplier : _) -> x / multiplier

-- -----------------------------------------------------------------
-- Patterns

numberWith :: (NumeralData -> t) -> (t -> Bool) -> PatternItem
numberWith f pred = Predicate $ \x ->
  case x of
    (Token Numeral x@NumeralData{}) -> pred (f x)
    _ -> False

numberBetween :: Double -> Double -> PatternItem
numberBetween low up = Predicate $ \x ->
  case x of
    (Token Numeral NumeralData {value = v, multipliable = False}) ->
      low <= v && v < up
    _ -> False

oneOf :: [Double] -> PatternItem
oneOf vs = Predicate $ \x ->
  case x of
    (Token Numeral NumeralData {value = v}) -> elem v vs
    _ -> False

-- -----------------------------------------------------------------
-- Production

withMultipliable :: Token -> Maybe Token
withMultipliable (Token Numeral x@NumeralData{}) =
  Just . Token Numeral $ x {multipliable = True}
withMultipliable _ = Nothing

withGrain :: Int -> Token -> Maybe Token
withGrain g (Token Numeral x@NumeralData{}) =
  Just . Token Numeral $ x {grain = Just g}
withGrain _ _ = Nothing

notOkForAnyTime :: Token -> Maybe Token
notOkForAnyTime (Token Numeral x) =
  Just . Token Numeral $ x {okForAnyTime = False}
notOkForAnyTime _ = Nothing

double :: Double -> Maybe Token
double x = Just . Token Numeral $ NumeralData
  { value = x
  , grain = Nothing
  , multipliable = False
  , okForAnyTime = True
  }

integer :: Integer -> Maybe Token
integer = double . fromIntegral

multiply :: Token -> Token -> Maybe Token
multiply
  (Token Numeral (NumeralData {value = v1}))
  (Token Numeral (NumeralData {value = v2, grain = g})) = case g of
  Nothing -> double $ v1 * v2
  Just grain | v2 > v1 -> double (v1 * v2) >>= withGrain grain
             | otherwise -> Nothing
multiply _ _ = Nothing

divide :: Token -> Token -> Maybe Token
divide
  (Token Numeral (NumeralData {value = v1}))
  (Token Numeral (NumeralData {value = v2})) = case v1 / v2 of
    x | isInfinite x || isNaN x -> Nothing
    x -> double x
divide _ _ = Nothing

parseDecimal :: Bool -> Text -> Maybe Token
parseDecimal isDot match
  | isDot = parseDouble match >>= double
  | otherwise =
    parseDouble (Text.replace comma dot match)
    >>= double
