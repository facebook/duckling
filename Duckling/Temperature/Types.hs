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

module Duckling.Temperature.Types where

import Control.DeepSeq
import Data.Aeson
import Data.Hashable
import Data.Text (Text)
import GHC.Generics
import Prelude
import qualified Data.HashMap.Strict as H
import qualified Data.Text as Text

import Duckling.Resolve (Resolve(..))

data TemperatureUnit =
  Degree | Celsius | Fahrenheit
  deriving (Eq, Generic, Hashable, Show, Ord, NFData)

instance ToJSON TemperatureUnit where
  toJSON = String . Text.toLower . Text.pack . show

data TemperatureData = TemperatureData
  { unit     :: Maybe TemperatureUnit
  , value    :: Maybe Int
  , minValue :: Maybe Int
  , maxValue :: Maybe Int
  } deriving (Eq, Generic, Hashable, Show, Ord, NFData)

instance Resolve TemperatureData where
  type ResolvedValue TemperatureData = TemperatureValue
  resolve _ _ TemperatureData {unit = Nothing} = Nothing
  resolve _ _ TemperatureData {unit = Just unit, value = Just value} =
    Just (simple unit value, False)
  resolve _ _ TemperatureData {unit = Just unit, minValue = Just from
                              , maxValue = Just to} =
    Just (between unit (from, to), False)
  resolve _ _ TemperatureData {unit = Just unit, minValue = Just from} =
    Just (above unit from, False)
  resolve _ _ TemperatureData {unit = Just unit, maxValue = Just to} =
    Just (under unit to, False)
  resolve _ _ _ = Nothing

data IntervalDirection = Above | Under
  deriving (Eq, Generic, Hashable, Ord, Show, NFData)

data SingleValue = SingleValue
  { vUnit :: TemperatureUnit
  , vValue :: Int
  }
  deriving (Eq, Show)

instance ToJSON SingleValue where
  toJSON (SingleValue unit value) = object
    [ "value" .= value
    , "unit"  .= unit
    ]

data TemperatureValue
  = SimpleValue SingleValue
  | IntervalValue (SingleValue, SingleValue)
  | OpenIntervalValue (SingleValue, IntervalDirection)
  deriving (Show, Eq)

instance ToJSON TemperatureValue where
  toJSON (SimpleValue value) = case toJSON value of
    Object o -> Object $ H.insert "type" (toJSON ("value" :: Text)) o
    _        -> Object H.empty
  toJSON (IntervalValue (from, to)) = object
    [ "type" .= ("interval" :: Text)
    , "from" .= toJSON from
    , "to"   .= toJSON to
    ]
  toJSON (OpenIntervalValue (from, Above)) = object
    [ "type" .= ("interval" :: Text)
    , "from" .= toJSON from
    ]
  toJSON (OpenIntervalValue (to, Under)) = object
    [ "type" .= ("interval" :: Text)
    , "to"   .= toJSON to
    ]

-- -----------------------------------------------------------------
-- Value helpers

simple :: TemperatureUnit -> Int -> TemperatureValue
simple u v = SimpleValue $ single u v

between :: TemperatureUnit -> (Int, Int) -> TemperatureValue
between u (from, to) = IntervalValue (single u from, single u to)

above :: TemperatureUnit -> Int -> TemperatureValue
above = openInterval Above

under :: TemperatureUnit -> Int -> TemperatureValue
under = openInterval Under

openInterval :: IntervalDirection -> TemperatureUnit -> Int -> TemperatureValue
openInterval direction u v = OpenIntervalValue (single u v, direction)

single :: TemperatureUnit -> Int -> SingleValue
single u v = SingleValue {vUnit = u, vValue = v}

unitsAreCompatible :: Maybe TemperatureUnit -> TemperatureUnit -> Bool
unitsAreCompatible (Just u1) u2 = u1 == u2
unitsAreCompatible Nothing _ = True
