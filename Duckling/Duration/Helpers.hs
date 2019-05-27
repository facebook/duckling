-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}

module Duckling.Duration.Helpers
  ( duration
  , isGrain
  , isNatural
  , minutesFromHourMixedFraction
  , timesOneAndAHalf
  ) where

import Prelude

import Duckling.Dimensions.Types
import Duckling.Duration.Types (DurationData (DurationData))
import Duckling.Numeral.Helpers (isNatural)
import Duckling.Types
import qualified Duckling.Duration.Types as TDuration
import qualified Duckling.TimeGrain.Types as TG

-- -----------------------------------------------------------------
-- Patterns

isGrain :: TG.Grain -> Predicate
isGrain value (Token TimeGrain grain) = grain == value
isGrain _ _ = False

-- -----------------------------------------------------------------
-- Production

duration :: TG.Grain -> Int -> DurationData
duration grain n = DurationData {TDuration.grain = grain, TDuration.value = n}

minutesFromHourMixedFraction :: Integer -> Integer -> Integer -> DurationData
minutesFromHourMixedFraction h n d =
  duration TG.Minute $ fromIntegral $ 60 * h + quot (n * 60) d

timesOneAndAHalf :: TG.Grain -> Int -> Maybe DurationData
timesOneAndAHalf grain = case grain of
  TG.Minute -> Just . duration TG.Second . (30+) . (60*)
  TG.Hour   -> Just . duration TG.Minute . (30+) . (60*)
  TG.Day    -> Just . duration TG.Hour   . (12+) . (24*)
  TG.Month  -> Just . duration TG.Day    . (15+) . (30*)
  TG.Year   -> Just . duration TG.Month  . (6+)  . (12*)
  _         -> const Nothing
