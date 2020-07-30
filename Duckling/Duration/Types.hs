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

import qualified Data.HashMap.Strict as H
import Duckling.Resolve (Resolve(..))
import Duckling.TimeGrain.Types (Grain(..), inSeconds)

data DurationData = DurationData
  { value :: Maybe Int
  , grain :: Grain
  , minValue :: Maybe Int
  , maxValue :: Maybe Int
  }
  deriving (Eq, Generic, Hashable, Show, Ord, NFData)

instance Resolve DurationData where
  type ResolvedValue DurationData = DurationValue
  resolve _ _ DurationData {value = Just val, grain} =
    Just (simple val grain, False)
  resolve _ _ DurationData {value = Nothing, grain
                         , minValue = Just from, maxValue = Just to} =
    Just (between (from, to) grain, False)
  resolve _ _ DurationData {value = Nothing, grain
                         , minValue = Just from, maxValue = Nothing} =
    Just (above from grain, False)
  resolve _ _ DurationData {value = Nothing, grain
                         , minValue = Nothing, maxValue = Just to} =
    Just (under to grain, False)
  resolve _ _ _ = Nothing

-- instance Resolve DurationData where
--   type ResolvedValue DurationData = DurationData
--   resolve _ _ x = Just (x, False)

data IntervalDirection = Above | Under
  deriving (Eq, Generic, Hashable, Ord, Show, NFData)

data SingleValue = SingleValue
  { vValue :: Int
  , vGrain :: Grain
  }
  deriving (Eq, Ord, Show)

data DurationValue
  = SimpleValue SingleValue
  | IntervalValue (SingleValue, SingleValue)
  | OpenIntervalValue (SingleValue, IntervalDirection)
  deriving (Eq, Ord, Show)

instance Semigroup DurationData where
  d1@(DurationData {grain = g1}) <> d2@(DurationData {grain = g2}) = 
    DurationData {value = Just (v1 + v2), grain = g, minValue = Nothing, maxValue = Nothing}
    where
    g = g1 `min` g2
    (DurationData {value = Just v1}, DurationData {value = Just v2}) = both (withGrain g) (d1,d2)

instance ToJSON SingleValue where
  toJSON (SingleValue value grain) = object
    [ "type"       .= ("value" :: Text)
    , "value"      .= value
    , "unit"       .= grain
    , showt grain  .= value
    , "normalized" .= object
      [ "unit"  .= ("second" :: Text)
      , "value" .= inSeconds grain value
      ]
    ]

instance ToJSON DurationValue where
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

-- | Convert a duration to the given grain, rounded to the
-- nearest integer. For example, 1 month is converted to 4 weeks.
withGrain :: Grain -> DurationData -> DurationData
withGrain g d@(DurationData {value = Just v1, grain = g1})
  | g == g1 = d
  | otherwise = DurationData{value = Just v, grain = g, minValue = Nothing, maxValue = Nothing}
      where
      v = round $ inSeconds g1 (fromIntegral v1 :: Double) / inSeconds g 1

-- -----------------------------------------------------------------
-- Value helpers

simple :: Int -> Grain -> DurationValue
simple v g = SimpleValue $ single v g

between :: (Int, Int) -> Grain -> DurationValue
between (from, to) g = IntervalValue (single from g, single to g)

above :: Int -> Grain -> DurationValue
above = openInterval Above

under :: Int -> Grain -> DurationValue
under = openInterval Under

openInterval :: IntervalDirection -> Int -> Grain -> DurationValue
openInterval direction v g = OpenIntervalValue (single v g, direction)

single :: Int -> Grain -> SingleValue
single v g = SingleValue {vValue = v, vGrain = g}