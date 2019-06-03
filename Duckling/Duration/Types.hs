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

module Duckling.Duration.Types where

import Control.DeepSeq
import Data.Aeson
import Data.Hashable
import Data.Semigroup
import Data.Text (Text)
import Data.Tuple.Extra (both)
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

instance Semigroup DurationData where
  d1@(DurationData _ g1) <> d2@(DurationData _ g2) = DurationData (v1+v2) g
    where
    g = g1 `min` g2
    (DurationData v1 _, DurationData v2 _) = both (withGrain g) (d1,d2)

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

-- | Convert a duration to the given grain, rounded to the
-- nearest integer. For example, 1 month is converted to 4 weeks.
withGrain :: Grain -> DurationData -> DurationData
withGrain g d@(DurationData v1 g1)
  | g == g1 = d
  | otherwise = DurationData v g
      where
      v = round $ inSeconds g1 (fromIntegral v1 :: Double) / inSeconds g 1
