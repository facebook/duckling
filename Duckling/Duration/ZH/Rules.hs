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

import Prelude
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Duration.Helpers
import Duckling.Duration.Types (DurationData(..))
import Duckling.Numeral.Helpers (parseInteger, decimalsToDouble)
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
      (Token Numeral NumeralData{TNumeral.value = v}:_) ->
        Just $ Token Duration $ duration TG.Minute $ floor v * 5
      _ -> Nothing
  }

ruleDurationHalfATimeGrain :: Rule
ruleDurationHalfATimeGrain = Rule
  { name = "half a <time-grain>"
  , pattern =
    [ regex "半"
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
      (Token Numeral NumeralData{TNumeral.value = v}:Token TimeGrain grain:_) -> do
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
      (Token Numeral NumeralData{TNumeral.value = v}:_:Token TimeGrain grain:_) -> do
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
        Just $ Token Duration $ minutesFromHourMixedFraction hh mnum mden
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
      (Token Numeral NumeralData{TNumeral.value = m}:
       _:
       Token Numeral NumeralData{TNumeral.value = s}:
       _) -> Just
        $ Token Duration
        $ duration TG.Minute
        $ floor ((m + decimalsToDouble s) * 60)
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
      (Token Numeral NumeralData{TNumeral.value = m}:
       _:
       Token Numeral NumeralData{TNumeral.value = s}:
       _) -> Just
        $ Token Duration
        $ duration TG.Second
        $ floor ((m + decimalsToDouble s) * 60)
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
      (Token Numeral h:
       _:
       Token Numeral m:
       _) -> Just
        $ Token Duration
        $ duration TG.Minute
        $ floor (TNumeral.value m) + 60 * floor (TNumeral.value h)
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
      (Token Numeral m:
       _:
       Token Numeral s:
       _) -> Just
        $ Token Duration
        $ duration TG.Second
        $ floor (TNumeral.value s) + 60 * floor (TNumeral.value m)
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
      (Token Numeral h:
       _:
       Token Numeral s:
       _) -> Just
         $ Token Duration
         $ duration TG.Second
         $ floor (TNumeral.value s) + 3600 * floor (TNumeral.value h)
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
      (Token Numeral h:
       _:
       Token Numeral m:
       _:
       Token Numeral s:
       _) -> Just
         $ Token Duration
         $ duration TG.Second
         $          floor (TNumeral.value s)
           + 60   * floor (TNumeral.value m)
           + 3600 * floor (TNumeral.value h)
      _ -> Nothing
  }

ruleCompositeDurationAnd :: Rule
ruleCompositeDurationAnd = Rule
  { name = "composite <duration> and <duration>"
  , pattern =
    [ dimension Duration
    , regex "零"
    , dimension Duration
    ]
  , prod = \case
      (Token Duration DurationData{TDuration.value = v, TDuration.grain = g}:
       _:
       Token Duration dd@DurationData{TDuration.grain = dg}:
       _) | g > dg -> Just $ Token Duration $ duration g v <> dd
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
  ]
