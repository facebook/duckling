-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


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
import qualified Data.Text as Text
import GHC.Generics
import Prelude

import Duckling.Resolve (Resolve(..))

data TemperatureUnit =
  Degree | Celsius | Fahrenheit
  deriving (Eq, Generic, Hashable, Show, Ord, NFData)

instance ToJSON TemperatureUnit where
  toJSON = String . Text.toLower . Text.pack . show

data TemperatureData = TemperatureData
  { unit :: Maybe TemperatureUnit
  , value :: Int
  } deriving (Eq, Generic, Hashable, Show, Ord, NFData)

instance Resolve TemperatureData where
  type ResolvedValue TemperatureData = TemperatureValue
  resolve _ _ TemperatureData {unit = Nothing} = Nothing
  resolve _ _ TemperatureData {unit = Just unit, value} =
    Just (TemperatureValue {vUnit = unit, vValue = value}, False)

data TemperatureValue = TemperatureValue
  { vUnit :: TemperatureUnit
  , vValue :: Int
  } deriving (Eq, Show)

instance ToJSON TemperatureValue where
  toJSON (TemperatureValue unit value) = object
    [ "type" .= ("value" :: Text)
    , "value" .= value
    , "unit" .= unit
    ]
