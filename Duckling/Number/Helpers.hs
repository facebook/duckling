-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Number.Helpers
  ( decimalsToDouble
  , double
  , integer
  , multiply
  , numberBetween
  , numberWith
  , oneOf
  , parseDouble
  , parseInt
  , withGrain
  , withMultipliable
  , parseDecimal,
  ) where

import qualified Data.Attoparsec.Text as Atto
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Prelude

import Duckling.Dimensions.Types
import Duckling.Number.Types (NumberData (..))
import qualified Duckling.Number.Types as TNumber
import Duckling.Types
zeroT :: Text
zeroT = Text.singleton '0'

dot :: Text
dot = Text.singleton '.'

comma :: Text
comma = Text.singleton ','

parseInt :: Text -> Maybe Int
parseInt =
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

numberWith :: (NumberData -> t) -> (t -> Bool) -> PatternItem
numberWith f pred = Predicate $ \x ->
  case x of
    (Token DNumber x@NumberData{}) -> pred (f x)
    _ -> False

numberBetween :: Double -> Double -> PatternItem
numberBetween low up = Predicate $ \x ->
  case x of
    (Token DNumber NumberData {TNumber.value = v, TNumber.multipliable = False}) ->
      low <= v && v < up
    _ -> False

oneOf :: [Double] -> PatternItem
oneOf vs = Predicate $ \x ->
  case x of
    (Token DNumber NumberData {TNumber.value = v}) -> elem v vs
    _ -> False

-- -----------------------------------------------------------------
-- Production

withMultipliable :: Token -> Maybe Token
withMultipliable (Token DNumber x@NumberData{}) =
  Just . Token DNumber $ x {TNumber.multipliable = True}
withMultipliable _ = Nothing

withGrain :: Int -> Token -> Maybe Token
withGrain g (Token DNumber x@NumberData{}) =
  Just . Token DNumber $ x {TNumber.grain = Just g}
withGrain _ _ = Nothing

double :: Double -> Maybe Token
double x = Just . Token DNumber $ NumberData
  { TNumber.value = x
  , TNumber.grain = Nothing
  , TNumber.multipliable = False
  }

integer :: Integer -> Maybe Token
integer = double . fromIntegral

multiply :: Token -> Token -> Maybe Token
multiply
  (Token DNumber (NumberData {TNumber.value = v1}))
  (Token DNumber (NumberData {TNumber.value = v2, TNumber.grain = g})) = case g of
  Nothing -> double $ v1 * v2
  Just grain | v2 > v1 -> double (v1 * v2) >>= withGrain grain
             | otherwise -> Nothing
multiply _ _ = Nothing

parseDecimal :: Bool -> Text -> Maybe Token
parseDecimal isDot match
  | isDot = parseDouble match >>= double
  | otherwise =
    parseDouble (Text.replace comma dot match)
    >>= double
