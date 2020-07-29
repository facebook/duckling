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
{-# LANGUAGE TypeFamilies #-}

module Duckling.Numeral.Types where

import Control.DeepSeq
import Data.Aeson
import Data.Hashable
import Data.Maybe
import Data.Text (Text)
import GHC.Generics
import Prelude
import qualified Data.HashMap.Strict as H
import Duckling.Resolve

data NumeralData = NumeralData
  { value        :: Maybe Double
  , grain        :: Maybe Int
  , multipliable :: Bool
  -- Hack until other use cases pop up,
  -- at which point we'll craft a generic solution.
  -- This allows to explicitly flag Numerals that don't work well with Time.
  -- See `ruleTODLatent`. Prevents things like "at single", "dozens o'clock".
  , okForAnyTime :: Bool
  , minValue :: Maybe Double
  , maxValue :: Maybe Double
  }
  deriving (Eq, Generic, Hashable, Ord, Show, NFData)

instance Resolve NumeralData where
  type ResolvedValue NumeralData = NumeralValue
  resolve _ _ NumeralData {value = Just v} = 
    Just (simple v, False)
  resolve _ _ NumeralData {minValue = Just from, maxValue = Just to} =
    Just (between (from, to), False)
  resolve _ _ NumeralData {minValue = Just v, maxValue = Nothing} =
    Just (above v, False)
  resolve _ _ NumeralData {minValue = Nothing, maxValue = Just v} =
    Just (under v, False)
  resolve _ _ _ = Nothing

data IntervalDirection = Above | Under
  deriving (Eq, Generic, Hashable, Ord, Show, NFData)

-- newtype NumeralValue = NumeralValue { vValue :: Double }
--   deriving (Eq, Show)

data SingleValue = SingleValue
  { vValue :: Double
  }
  deriving (Eq, Ord, Show)

instance ToJSON SingleValue where
  toJSON (SingleValue value) = object
    [ "type" .= ("value" :: Text)
    , "value" .= value
    ]

data NumeralValue
  = SimpleValue SingleValue
  | IntervalValue (SingleValue, SingleValue)
  | OpenIntervalValue (SingleValue, IntervalDirection)
  deriving (Eq, Ord, Show)

instance ToJSON NumeralValue where
  toJSON (SimpleValue value) = case toJSON value of
    Object o -> Object $ H.insert "type" (toJSON ("value" :: Text)) o
    _ -> Object H.empty
  toJSON (IntervalValue (from, to)) = object
    [ "type" .= ("interval" :: Text)
    , "from" .= toJSON from
    , "to" .= toJSON to
    ]
  toJSON (OpenIntervalValue (from, Above)) = object
    [ "type" .= ("interval" :: Text)
    , "from" .= toJSON from
    ]
  toJSON (OpenIntervalValue (to, Under)) = object
    [ "type" .= ("interval" :: Text)
    , "to" .= toJSON to
    ]

getIntValue :: Double -> Maybe Int
getIntValue x = if rest == 0 then Just int else Nothing
  where
    (int, rest) = properFraction x :: (Int, Double)

isInteger :: Double -> Bool
isInteger = isJust . getIntValue

isIntegerBetween :: Double -> Int -> Int -> Bool
isIntegerBetween x low high = case getIntValue x of
  Just int -> low <= int && int <= high
  Nothing -> False

-- -----------------------------------------------------------------
-- Value helpers

simple :: Double -> NumeralValue
simple v = SimpleValue $ single v

between :: (Double, Double) -> NumeralValue
between (from, to) = IntervalValue (single from, single to)

above :: Double -> NumeralValue
above = openInterval Above

under :: Double -> NumeralValue
under = openInterval Under

openInterval :: IntervalDirection -> Double -> NumeralValue
openInterval direction v = OpenIntervalValue (single v, direction)

single :: Double -> SingleValue
single v = SingleValue {vValue = v}