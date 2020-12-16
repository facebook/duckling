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
  , nPlusOneHalf
  , secondsFromHourMixedFraction
  , timesOneQuarter
  , timesThreeQuarter
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

nPlusOneHalf :: TG.Grain -> Int -> Maybe DurationData
nPlusOneHalf grain = case grain of
  TG.Minute -> Just . duration TG.Second . (30+) . (60*)
  TG.Hour   -> Just . duration TG.Minute . (30+) . (60*)
  TG.Day    -> Just . duration TG.Hour   . (12+) . (24*)
  TG.Month  -> Just . duration TG.Day    . (15+) . (30*)
  TG.Year   -> Just . duration TG.Month  . (6+)  . (12*)
  _         -> const Nothing

secondsFromHourMixedFraction :: Integer -> Integer -> Integer -> DurationData
secondsFromHourMixedFraction m s d =
  duration TG.Second $ fromIntegral $ 60 * m + quot (s * 60) d

timesOneQuarter :: TG.Grain -> Int -> Maybe DurationData
timesOneQuarter grain = case grain of
  TG.Minute -> Just . duration TG.Second . (15+) . (60*)
  TG.Hour   -> Just . duration TG.Minute . (15+) . (60*)
  TG.Day    -> Just . duration TG.Hour   . (6+) . (24*)
  TG.Month  -> Just . duration TG.Day    . (7+) . (30*)
  TG.Year   -> Just . duration TG.Month  . (3+)  . (12*)
  _         -> const Nothing

timesThreeQuarter :: TG.Grain -> Int -> Maybe DurationData
timesThreeQuarter grain = case grain of
  TG.Minute -> Just . duration TG.Second . (45+) . (60*)
  TG.Hour   -> Just . duration TG.Minute . (45+) . (60*)
  TG.Day    -> Just . duration TG.Hour   . (18+) . (24*)
  TG.Month  -> Just . duration TG.Day    . (21+) . (30*)
  TG.Year   -> Just . duration TG.Month  . (9+)  . (12*)
  _         -> const Nothing
