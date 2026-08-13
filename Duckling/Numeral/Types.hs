-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module Duckling.Numeral.Types
  ( NumeralData(..)
  , NumeralValue(.., NumeralValue, vValue)
  , getIntValue
  , isInteger
  , isMultiple
  , isIntegerBetween
  ) where

import Control.DeepSeq
import Data.Aeson
import Data.Hashable
import Data.Maybe
import Data.Text (Text)
import GHC.Generics
import Prelude

import Duckling.Resolve

data NumeralData = NumeralData
  { value        :: Double
  , grain        :: Maybe Int
  , multipliable :: Bool
  -- Hack until other use cases pop up,
  -- at which point we'll craft a generic solution.
  -- This allows to explicitly flag Numerals that don't work well with Time.
  -- See `ruleTODLatent`. Prevents things like "at single", "dozens o'clock".
  , okForAnyTime :: Bool
  , rangeValue   :: Maybe (Double, Double)
  }
  deriving (Eq, Generic, Hashable, Ord, Show, NFData)

instance Resolve NumeralData where
  type ResolvedValue NumeralData = NumeralValue
  resolve _ _ NumeralData {value, rangeValue} = Just (val, False)
    where
      val = case rangeValue of
        Nothing -> NumeralSingleValue value
        Just (fromVal, toVal) -> NumeralIntervalValue (fromVal, toVal)

data NumeralValue
  = NumeralSingleValue { vValue :: Double }
  | NumeralIntervalValue (Double, Double)
  deriving (Eq, Show)

pattern NumeralValue :: Double -> NumeralValue
pattern NumeralValue v = NumeralSingleValue v

instance ToJSON NumeralValue where
  toJSON (NumeralSingleValue value) = object
    [ "type" .= ("value" :: Text)
    , "value" .= value
    ]
  toJSON (NumeralIntervalValue (fromVal, toVal)) = object
    [ "type" .= ("interval" :: Text)
    , "from" .= object [ "value" .= fromVal ]
    , "to"   .= object [ "value" .= toVal ]
    ]

getIntValue :: Double -> Maybe Int
getIntValue x = if rest == 0 then Just int else Nothing
  where
    (int, rest) = properFraction x :: (Int, Double)

isInteger :: Double -> Bool
isInteger = isJust . getIntValue

isMultiple :: Int -> Double -> Bool
isMultiple base = isJust . getIntValue . (/ fromIntegral base)

isIntegerBetween :: Double -> Int -> Int -> Bool
isIntegerBetween x low high = case getIntValue x of
  Just int -> low <= int && int <= high
  Nothing -> False
