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

module Duckling.Duration.Types where

import Control.DeepSeq
import Data.Aeson
import Data.Hashable
import Data.Text (Text)
import GHC.Generics
import TextShow (showt)
import Prelude

import Duckling.Resolve (Resolve(..))
import Duckling.TimeGrain.Types (Grain(..), inSeconds)

data DurationData = DurationData
  { value :: Int
  , grain :: Grain
  }
  deriving (Eq, Generic, Hashable, Show, Ord, NFData)

instance Resolve DurationData where
  type ResolvedValue DurationData = DurationData
  resolve _ _ x = Just (x, False)

instance ToJSON DurationData where
  toJSON DurationData {value, grain} = object
    [ "type"       .= ("value" :: Text)
    , "value"      .= value
    , "unit"       .= grain
    , showt grain  .= value
    , "normalized" .= object
      [ "unit"  .= ("second" :: Text)
      , "value" .= inSeconds grain value
      ]
    ]
