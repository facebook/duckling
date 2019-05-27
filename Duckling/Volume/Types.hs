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

module Duckling.Volume.Types where

import Control.DeepSeq
import Data.Aeson
import Data.Hashable
import Data.Text (Text)
import GHC.Generics
import Prelude
import Duckling.Resolve (Resolve (..))
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as H

data Unit
  = Gallon
  | Hectolitre
  | Litre
  | Millilitre
  deriving (Eq, Generic, Hashable, Ord, Show, NFData)

instance ToJSON Unit where
  toJSON = String . Text.toLower . Text.pack . show

data VolumeData = VolumeData
  { value :: Maybe Double
  , unit :: Maybe Unit
  , minValue :: Maybe Double
  , maxValue :: Maybe Double
  }
  deriving (Eq, Generic, Hashable, Ord, Show, NFData)

instance Resolve VolumeData where
  type ResolvedValue VolumeData = VolumeValue
  resolve _ _ VolumeData {value = Just v, unit = Just u} =
    Just (simple u v, False)
  resolve _ _ VolumeData {value = Nothing, unit = Just u
                         , minValue = Just from, maxValue = Just to} =
    Just (between u (from, to), False)
  resolve _ _ VolumeData {value = Nothing, unit = Just u
                         , minValue = Just v, maxValue = Nothing} =
    Just (above u v, False)
  resolve _ _ VolumeData {value = Nothing, unit = Just u
                         , minValue = Nothing, maxValue = Just v} =
    Just (under u v, False)
  resolve _ _ _ = Nothing

data IntervalDirection = Above | Under
  deriving (Eq, Generic, Hashable, Ord, Show, NFData)

data SingleValue = SingleValue
  { vUnit :: Unit
  , vValue :: Double
  }
  deriving (Eq, Show)

instance ToJSON SingleValue where
  toJSON SingleValue {vUnit, vValue} = object
    [ "value" .= vValue
    , "unit"  .= vUnit
    ]

data VolumeValue
  = SimpleValue SingleValue
  | IntervalValue (SingleValue, SingleValue)
  | OpenIntervalValue (SingleValue, IntervalDirection)
  deriving (Show, Eq)

instance ToJSON VolumeValue where
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

simple :: Unit -> Double -> VolumeValue
simple u v = SimpleValue $ single u v

between :: Unit -> (Double, Double) -> VolumeValue
between u (from,to) = IntervalValue (single u from, single u to)

above :: Unit -> Double -> VolumeValue
above = openInterval Above

under :: Unit -> Double -> VolumeValue
under = openInterval Under

openInterval :: IntervalDirection -> Unit -> Double -> VolumeValue
openInterval direction u v = OpenIntervalValue (single u v, direction)

single :: Unit -> Double -> SingleValue
single u v = SingleValue {vUnit = u, vValue = v}
