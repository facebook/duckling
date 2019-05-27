-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.EN.Rules
  ( rules
  ) where

import Data.Semigroup ((<>))
import Data.String
import Prelude
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Duration.Helpers
import Duckling.Duration.Types (DurationData(..))
import Duckling.Numeral.Helpers (parseInt, parseInteger)
import Duckling.Numeral.Types (NumeralData(..))
import Duckling.Regex.Types
import Duckling.Types
import qualified Duckling.Duration.Types as TDuration
import qualified Duckling.Numeral.Types as TNumeral
import qualified Duckling.TimeGrain.Types as TG

ruleDurationQuarterOfAnHour :: Rule
ruleDurationQuarterOfAnHour = Rule
  { name = "quarter of an hour"
  , pattern =
    [ regex "(1/4\\s?h(our)?|(a\\s)?quarter of an hour)"
    ]
  , prod = \_ -> Just . Token Duration $ duration TG.Minute 15
  }

ruleDurationHalfAnHourAbbrev :: Rule
ruleDurationHalfAnHourAbbrev = Rule
  { name = "half an hour (abbrev)."
  , pattern =
    [ regex "1/2\\s?h"
    ]
  , prod = \_ -> Just . Token Duration $ duration TG.Minute 30
  }

ruleDurationThreeQuartersOfAnHour :: Rule
ruleDurationThreeQuartersOfAnHour = Rule
  { name = "three-quarters of an hour"
  , pattern =
    [ regex "(3/4\\s?h(our)?|three(\\s|-)quarters of an hour)"
    ]
  , prod = \_ -> Just . Token Duration $ duration TG.Minute 45
  }

ruleDurationFortnight :: Rule
ruleDurationFortnight = Rule
  { name = "fortnight"
  , pattern =
    [ regex "(a|one)? fortnight"
    ]
  , prod = \_ -> Just . Token Duration $ duration TG.Day 14
  }

ruleNumeralQuotes :: Rule
ruleNumeralQuotes = Rule
  { name = "<integer> + '\""
  , pattern =
    [ Predicate isNatural
    , regex "(['\"])"
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = v}:
       Token RegexMatch (GroupMatch (x:_)):
       _) -> case x of
         "'"  -> Just . Token Duration . duration TG.Minute $ floor v
         "\"" -> Just . Token Duration . duration TG.Second $ floor v
         _    -> Nothing
      _ -> Nothing
  }

ruleDurationNumeralMore :: Rule
ruleDurationNumeralMore = Rule
  { name = "<integer> more <unit-of-duration>"
  , pattern =
    [ Predicate isNatural
    , regex "more|less"
    , dimension TimeGrain
    ]
  , prod = \case
      (Token Numeral nd:_:Token TimeGrain grain:_) ->
        Just . Token Duration . duration grain . floor $ TNumeral.value nd
      _ -> Nothing
  }

ruleDurationDotNumeralHours :: Rule
ruleDurationDotNumeralHours = Rule
  { name = "number.number hours"
  , pattern =
    [ regex "(\\d+)\\.(\\d+) *hours?"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (h:m:_)):_) -> do
        hh <- parseInteger h
        mnum <- parseInteger m
        let mden = 10 ^ Text.length m
        Just . Token Duration $ minutesFromHourMixedFraction hh mnum mden
      _ -> Nothing
  }

ruleDurationAndHalfHour :: Rule
ruleDurationAndHalfHour = Rule
  { name = "<integer> and an half hour"
  , pattern =
    [ Predicate isNatural
    , regex "and (an? )?half hours?"
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = v}:_) ->
        Just . Token Duration . duration TG.Minute $ 30 + 60 * floor v
      _ -> Nothing
  }

ruleDurationA :: Rule
ruleDurationA = Rule
  { name = "a <unit-of-duration>"
  , pattern =
    [ regex "an?"
    , dimension TimeGrain
    ]
  , prod = \case
      (_:Token TimeGrain grain:_) -> Just . Token Duration $ duration grain 1
      _ -> Nothing
  }

ruleDurationHalfATimeGrain :: Rule
ruleDurationHalfATimeGrain = Rule
  { name = "half a <time-grain>"
  , pattern =
    [ regex "(1/2|half)( an?)?"
    , dimension TimeGrain
    ]
  , prod = \case
      (_:Token TimeGrain grain:_) -> Token Duration <$> timesOneAndAHalf grain 0
      _ -> Nothing
  }

ruleDurationOneGrainAndHalf :: Rule
ruleDurationOneGrainAndHalf = Rule
  { name = "a <unit-of-duration> and a half"
  , pattern =
    [ regex "an?|one"
    , dimension TimeGrain
    , regex "and (a )?half"
    ]
  , prod = \case
      (_:Token TimeGrain grain:_) -> Token Duration <$> timesOneAndAHalf grain 1
      _ -> Nothing
  }

ruleDurationPrecision :: Rule
ruleDurationPrecision = Rule
  { name = "about|exactly <duration>"
  , pattern =
    [ regex "(about|around|approximately|exactly)"
    , dimension Duration
    ]
    , prod = \case
        (_:token:_) -> Just token
        _ -> Nothing
  }

-- | NOTE: Oxford comma is not supported.
ruleCompositeDurationCommasAnd :: Rule
ruleCompositeDurationCommasAnd = Rule
  { name = "composite <duration> (with ,/and)"
  , pattern =
    [ Predicate isNatural
    , dimension TimeGrain
    , regex ",|and"
    , dimension Duration
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = v}:
       Token TimeGrain g:
       _:
       Token Duration dd@DurationData{TDuration.grain = dg}:
       _) | g > dg -> Just . Token Duration $ duration g (floor v) <> dd
      _ -> Nothing
  }

ruleCompositeDuration :: Rule
ruleCompositeDuration = Rule
  { name = "composite <duration>"
  , pattern =
    [ Predicate isNatural
    , dimension TimeGrain
    , dimension Duration
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = v}:
       Token TimeGrain g:
       Token Duration dd@DurationData{TDuration.grain = dg}:
       _) | g > dg -> Just . Token Duration $ duration g (floor v) <> dd
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleDurationQuarterOfAnHour
  , ruleDurationHalfAnHourAbbrev
  , ruleDurationThreeQuartersOfAnHour
  , ruleDurationFortnight
  , ruleDurationNumeralMore
  , ruleDurationDotNumeralHours
  , ruleDurationAndHalfHour
  , ruleDurationA
  , ruleDurationHalfATimeGrain
  , ruleDurationOneGrainAndHalf
  , ruleDurationPrecision
  , ruleNumeralQuotes
  , ruleCompositeDuration
  , ruleCompositeDurationCommasAnd
  ]
