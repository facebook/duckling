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

module Duckling.Distance.Types where

import Control.DeepSeq
import Data.Aeson
import Data.Hashable
import Data.Text (Text)
import GHC.Generics
import Prelude
import qualified Data.HashMap.Strict as H
import qualified Data.Text as Text

import Duckling.Resolve (Resolve(..))

data Unit
  = Centimetre
  | Foot
  | Inch
  | Kilometre
  | M -- ambiguous between Mile and Metre
  | Metre
  | Mile
  | Millimetre
  | Yard
  deriving (Eq, Generic, Hashable, Ord, Show, NFData)

instance ToJSON Unit where
  toJSON = String . Text.toLower . Text.pack . show

data DistanceData = DistanceData
  { unit     :: Maybe Unit
  , value    :: Maybe Double
  , minValue :: Maybe Double
  , maxValue :: Maybe Double
  }
  deriving (Eq, Generic, Hashable, Ord, Show, NFData)

instance Resolve DistanceData where
  type ResolvedValue DistanceData = DistanceValue
  resolve _ _ DistanceData {unit = Just unit, value = Just val} =
    Just (simple unit val, False)
  resolve _ _ DistanceData {unit = Just unit, value = Nothing
                         , minValue = Just from, maxValue = Just to} =
    Just (between unit (from, to), False)
  resolve _ _ DistanceData {unit = Just unit, value = Nothing
                         , minValue = Just from, maxValue = Nothing} =
    Just (above unit from, False)
  resolve _ _ DistanceData {unit = Just unit, value = Nothing
                         , minValue = Nothing, maxValue = Just to} =
    Just (under unit to, False)
  resolve _ _ _ = Nothing

data IntervalDirection = Above | Under
  deriving (Eq, Generic, Hashable, Ord, Show, NFData)

data SingleValue = SingleValue
  { vUnit :: Unit
  , vValue :: Double
  }
  deriving (Eq, Ord, Show)

instance ToJSON SingleValue where
  toJSON (SingleValue unit value) = object
    [ "value" .= value
    , "unit"  .= unit
    ]

data DistanceValue
  = SimpleValue SingleValue
  | IntervalValue (SingleValue, SingleValue)
  | OpenIntervalValue (SingleValue, IntervalDirection)
  deriving (Eq, Ord, Show)

instance ToJSON DistanceValue where
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

-- -----------------------------------------------------------------
-- Value helpers

simple :: Unit -> Double -> DistanceValue
simple u v = SimpleValue $ single u v

between :: Unit -> (Double, Double) -> DistanceValue
between u (from, to) = IntervalValue (single u from, single u to)

above :: Unit -> Double -> DistanceValue
above = openInterval Above

under :: Unit -> Double -> DistanceValue
under = openInterval Under

openInterval :: IntervalDirection -> Unit -> Double -> DistanceValue
openInterval direction u v = OpenIntervalValue (single u v, direction)

single :: Unit -> Double -> SingleValue
single u v = SingleValue {vUnit = u, vValue = v}
