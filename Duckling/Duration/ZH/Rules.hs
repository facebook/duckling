-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.ZH.Rules
  ( rules
  ) where

import Data.Semigroup ((<>))
import Data.String
import Prelude
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Duration.Helpers
import Duckling.Duration.Types (DurationData(..))
import Duckling.Numeral.Helpers (parseInt, parseInteger, decimalsToDouble, isNumeralInterval)
import Duckling.Numeral.Types (NumeralData(..))
import Duckling.Regex.Types
import Duckling.Types
import qualified Duckling.Duration.Types as TDuration
import qualified Duckling.Numeral.Types as TNumeral
import qualified Duckling.TimeGrain.Types as TG

ruleDurationNFiveMinutes :: Rule
ruleDurationNFiveMinutes = Rule
  { name = "number of five minutes"
  , pattern =
    [ Predicate isNatural
    , regex "個字"
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = Just v}:_) -> 
        Just . Token Duration . duration TG.Minute $ floor(v) * 5
      _ -> Nothing
  }

ruleDurationHalfATimeGrain :: Rule
ruleDurationHalfATimeGrain = Rule
  { name = "half a <time-grain>"
  , pattern =
    [ regex "半(個|个)?"
    , dimension TimeGrain
    ]
  , prod = \case
      (_:Token TimeGrain grain:_) -> Token Duration <$> nPlusOneHalf grain 0
      _ -> Nothing
  }

ruleDurationOneGrainAndHalf :: Rule
ruleDurationOneGrainAndHalf = Rule
  { name = "a <unit-of-duration> and a half"
  , pattern =
    [ regex "一個半|個半"
    , dimension TimeGrain
    ]
  , prod = \case
      (_:Token TimeGrain grain:_) -> Token Duration <$> nPlusOneHalf grain 1
      _ -> Nothing
  }

ruleDurationOneGrainAndHalf2 :: Rule
ruleDurationOneGrainAndHalf2 = Rule
  { name = "a <unit-of-duration> and a half"
  , pattern =
    [ dimension TimeGrain
    , regex "半(鐘)?"
    ]
  , prod = \case
      (Token TimeGrain grain:_) -> Token Duration <$> nPlusOneHalf grain 1
      _ -> Nothing
  }

ruleDurationAndHalfGrain :: Rule
ruleDurationAndHalfGrain = Rule
  { name = "<integer> and a half <unit-of-duration>"
  , pattern =
    [ Predicate isNatural
    , dimension TimeGrain
    , regex "半(鐘)?"
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = Just v}:Token TimeGrain grain:_) -> do
        let vv = floor v
        Token Duration <$> nPlusOneHalf grain vv
      _ -> Nothing
  }

ruleDurationAndHalfGrain2 :: Rule
ruleDurationAndHalfGrain2 = Rule
  { name = "<integer> and a half <unit-of-duration>"
  , pattern =
    [ Predicate isNatural
    , regex "個半"
    , dimension TimeGrain
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = Just v}:_:Token TimeGrain grain:_) -> do
        let vv = floor v
        Token Duration <$> nPlusOneHalf grain vv
      _ -> Nothing
  }

ruleDurationDotNumeralHours :: Rule
ruleDurationDotNumeralHours = Rule
  { name = "number.number hours"
  , pattern =
    [ regex "(\\d+)\\.(\\d+)"
    , Predicate $ isGrain TG.Hour
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (h:m:_)):_) -> do
        hh <- parseInteger h
        mnum <- parseInteger m
        let mden = 10 ^ Text.length m
        Just . Token Duration $ minutesFromHourMixedFraction hh mnum mden
      _ -> Nothing
  }

ruleDurationDotNumeralHours2 :: Rule
ruleDurationDotNumeralHours2 = Rule
  { name = "number.number hours"
  , pattern =
    [ Predicate isNatural
    , regex "點"
    , Predicate isNatural
    , Predicate $ isGrain TG.Hour
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = Just m}:
       _:
       Token Numeral NumeralData{TNumeral.value = Just s}:
       _) -> Just . Token Duration . duration TG.Minute $
        floor ((m + decimalsToDouble (s)) * 60)
      _ -> Nothing
  }

ruleDurationDotNumeralMinutes :: Rule
ruleDurationDotNumeralMinutes = Rule
  { name = "number.number minutes"
  , pattern =
    [ regex "(\\d+)\\.(\\d+)"
    , Predicate $ isGrain TG.Minute
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (m:s:_)):_) -> do
        mm <- parseInteger m
        ss <- parseInteger s
        let sden = 10 ^ Text.length s
        Just $ Token Duration $ secondsFromHourMixedFraction mm ss sden
      _ -> Nothing
  }

ruleDurationDotNumeralMinutes2 :: Rule
ruleDurationDotNumeralMinutes2 = Rule
  { name = "number.number minutes"
  , pattern =
    [ Predicate isNatural
    , regex "點"
    , Predicate isNatural
    , Predicate $ isGrain TG.Minute
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = Just m}:
       _:
       Token Numeral NumeralData{TNumeral.value = Just s}:
       _) -> Just . Token Duration . duration TG.Second $
        floor ((m + decimalsToDouble (s)) * 60)
      _ -> Nothing
  }

ruleDurationHoursAndMinutes :: Rule
ruleDurationHoursAndMinutes = Rule
  { name = "<integer> hours and <integer> minutes"
  , pattern =
    [ Predicate isNatural
    , Predicate $ isGrain TG.Hour
    , Predicate isNatural
    , Predicate $ isGrain TG.Minute
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = Just h}:
       _:
       Token Numeral NumeralData{TNumeral.value = Just m}:
       _) -> Just . Token Duration . duration TG.Minute $
         (floor $ m) + 60 * floor (h)
      _ -> Nothing
  }

ruleDurationMinutesAndSeconds :: Rule
ruleDurationMinutesAndSeconds = Rule
  { name = "<integer> minutes and <integer> seconds"
  , pattern =
    [ Predicate isNatural
    , Predicate $ isGrain TG.Minute
    , Predicate isNatural
    , Predicate $ isGrain TG.Second
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = Just m}:
       _:
       Token Numeral NumeralData{TNumeral.value = Just s}:
       _) -> Just . Token Duration . duration TG.Second $
         (floor $ s) + 60 * floor (m)
      _ -> Nothing
  }

ruleDurationHoursAndSeconds :: Rule
ruleDurationHoursAndSeconds = Rule
  { name = "<integer> hours and <integer> seconds"
  , pattern =
    [ Predicate isNatural
    , Predicate $ isGrain TG.Hour
    , Predicate isNatural
    , Predicate $ isGrain TG.Second
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = Just h}:
       _:
       Token Numeral NumeralData{TNumeral.value = Just s}:
       _) -> Just . Token Duration . duration TG.Second $
         (floor $ s) + 3600 * floor (h)
      _ -> Nothing
  }

ruleDurationHoursAndMinutesAndSeconds :: Rule
ruleDurationHoursAndMinutesAndSeconds = Rule
  { name = "<integer> hours and <integer> minutes and <integer> seconds"
  , pattern =
    [ Predicate isNatural
    , Predicate $ isGrain TG.Hour
    , Predicate isNatural
    , Predicate $ isGrain TG.Minute
    , Predicate isNatural
    , Predicate $ isGrain TG.Second
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = Just h}:
       _:
       Token Numeral NumeralData{TNumeral.value = Just m}:
       _:
       Token Numeral NumeralData{TNumeral.value = Just s}:
       _) -> Just . Token Duration . duration TG.Second $
         (floor $ s) + 60 * floor (m) + 3600 * floor (h)
      _ -> Nothing
  }

ruleCompositeDurationAnd :: Rule
ruleCompositeDurationAnd = Rule
  { name = "composite <duration> and <duration>"
  , pattern =
    [ Predicate isSimpleDuration
    , regex "零"
    , Predicate isSimpleDuration
    ]
  , prod = \case
      (Token Duration DurationData{TDuration.value = Just v, TDuration.grain = g}:
       _:
       Token Duration dd@DurationData{TDuration.grain = dg}:
       _) | g > dg -> Just . Token Duration $ duration g (v) <> dd
      _ -> Nothing
  }

ruleIntervalNumeralDash :: Rule
ruleIntervalNumeralDash = Rule
  { name = "<numeral> - <duration>"
  , pattern =
    [ Predicate isNatural
    , regex "-|~|到|至|或"
    , Predicate isSimpleDuration
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = Just from}:
       _:Token Duration DurationData{TDuration.value = Just to,TDuration.grain = g}:
       _) | floor(from) < to ->
         Just . Token Duration . withInterval (floor(from), to) $ grainOnly g
      _ -> Nothing
  }

ruleIntervalDash :: Rule
ruleIntervalDash = Rule
  { name = "<duration> - <duration>"
  , pattern =
    [ Predicate isSimpleDuration
    , regex "-|~|到|至|或"
    , Predicate isSimpleDuration
    ]
  , prod = \case
      (Token Duration DurationData{TDuration.value = Just from,TDuration.grain = g1}:
       _:Token Duration DurationData{TDuration.value = Just to,TDuration.grain = g2}:
       _) | from < to && g1 == g2 ->
        Just . Token Duration . withInterval (from, to) $ grainOnly g1
      _ -> Nothing
  }

ruleIntervalFromNumeral :: Rule
ruleIntervalFromNumeral = Rule
  { name = "<numeral interval> duration"
  , pattern =
    [ Predicate isNumeralInterval
    , dimension TimeGrain
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.minValue = Just from, TNumeral.maxValue = Just to}:
        Token TimeGrain g:_) | from < to ->
        Just . Token Duration . withInterval (floor(from), floor(to)) $ grainOnly g
      _ -> Nothing
  }

ruleIntervalBound :: Rule
ruleIntervalBound = Rule
  { name = "under/less/lower/no more than <duration> (最多|至少|最少)"
  , pattern =
    [ regex "(最多|至少|最少|起碼)"
    , Predicate isSimpleDuration
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):
       Token Duration DurationData{TDuration.value = Just to,TDuration.grain = g}:
       _) -> case match of
        "最多" -> Just . Token Duration . withMax to $ grainOnly g
        "最少" -> Just . Token Duration . withMin to $ grainOnly g
        "至少" -> Just . Token Duration . withMin to $ grainOnly g
        "起碼" -> Just . Token Duration . withMin to $ grainOnly g
        _ -> Nothing
      _ -> Nothing
  }

ruleIntervalBound2 :: Rule
ruleIntervalBound2 = Rule
  { name = "under/less/lower/no more than <duration> (以下|以上)"
  , pattern =
    [ Predicate isSimpleDuration
    , regex "(以內|以上)"
    ]
  , prod = \case
      (Token Duration DurationData{TDuration.value = Just to,TDuration.grain = g}:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> case match of
        "以內" -> Just . Token Duration . withMax to $ grainOnly g
        "以上" -> Just . Token Duration . withMin to $ grainOnly g
        _ -> Nothing
      _ -> Nothing
  }

rulePrecision :: Rule
rulePrecision = Rule
  { name = "about <duration>"
  , pattern =
    [ dimension Duration
    , regex "左右"
    ]
  , prod = \case
      (token:_) -> Just token
      _ -> Nothing
  }


rules :: [Rule]
rules =
  [ ruleDurationNFiveMinutes
  , ruleDurationHalfATimeGrain
  , ruleDurationOneGrainAndHalf
  , ruleDurationOneGrainAndHalf2
  , ruleDurationAndHalfGrain
  , ruleDurationAndHalfGrain2
  , ruleDurationDotNumeralHours
  , ruleDurationDotNumeralHours2
  , ruleDurationDotNumeralMinutes
  , ruleDurationDotNumeralMinutes2
  , ruleDurationHoursAndMinutes
  , ruleDurationMinutesAndSeconds
  , ruleDurationHoursAndSeconds
  , ruleDurationHoursAndMinutesAndSeconds
  , ruleCompositeDurationAnd
  , ruleIntervalNumeralDash
  , ruleIntervalDash
  , ruleIntervalFromNumeral
  , ruleIntervalBound
  , ruleIntervalBound2
  , rulePrecision
  ]