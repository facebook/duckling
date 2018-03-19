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

module Duckling.Volume.Types where

import Control.DeepSeq
import Data.Aeson
import Data.Hashable
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics
import Prelude

import Duckling.Resolve (Resolve (..))

data Unit
  = Gallon
  | Hectolitre
  | Litre
  | Millilitre
  deriving (Eq, Generic, Hashable, Ord, Show, NFData)

instance ToJSON Unit where
  toJSON = String . Text.toLower . Text.pack . show

data VolumeData = VolumeData
  { unit :: Maybe Unit
  , value :: Double
  }
  deriving (Eq, Generic, Hashable, Ord, Show, NFData)

instance Resolve VolumeData where
  type ResolvedValue VolumeData = VolumeValue
  resolve _ _ VolumeData {unit = Nothing} = Nothing
  resolve _ _ VolumeData {unit = Just unit, value} = Just (VolumeValue
    {vValue = value, vUnit = unit}, False)

data VolumeValue = VolumeValue
  { vUnit :: Unit
  , vValue :: Double
  }
  deriving (Eq, Ord, Show)

instance ToJSON VolumeValue where
  toJSON (VolumeValue unit value) = object
    [ "type" .= ("value" :: Text)
    , "value" .= value
    , "unit" .= unit
    ]
