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

module Duckling.Distance.Types where

import Control.DeepSeq
import Data.Aeson
import Data.Hashable
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics
import Prelude

import Duckling.Resolve (Resolve(..))

data Unit
  = Foot
  | Centimetre
  | Kilometre
  | Inch
  | M -- ambiguous between Mile and Metre
  | Metre
  | Mile
  | Yard
  deriving (Eq, Generic, Hashable, Ord, Show, NFData)

instance ToJSON Unit where
  toJSON = String . Text.toLower . Text.pack . show

data DistanceData = DistanceData
  { unit :: Maybe Unit
  , value :: Double
  }
  deriving (Eq, Generic, Hashable, Ord, Show, NFData)

instance Resolve DistanceData where
  type ResolvedValue DistanceData = DistanceValue
  resolve _ DistanceData {unit = Nothing} = Nothing
  resolve _ DistanceData {unit = Just unit, value} =
    Just DistanceValue {vValue = value, vUnit = unit}

data DistanceValue = DistanceValue
  { vUnit :: Unit
  , vValue :: Double
  }
  deriving (Eq, Ord, Show)

instance ToJSON DistanceValue where
  toJSON (DistanceValue unit value) = object
    [ "type" .= ("value" :: Text)
    , "value" .= value
    , "unit" .= unit
    ]
