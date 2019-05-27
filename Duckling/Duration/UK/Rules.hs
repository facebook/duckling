-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.UK.Rules
  ( rules ) where

import Control.Monad (join)
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

ruleHalfAnHour :: Rule
ruleHalfAnHour = Rule
  { name = "half an hour"
  , pattern =
    [ regex "(1/2\\s?|пів\\s?)години?"
    ]
  , prod = \_ -> Just . Token Duration $ duration TG.Minute 30
  }

ruleNumeralnumberHours :: Rule
ruleNumeralnumberHours = Rule
  { name = "number.number hours"
  , pattern =
    [ regex "(\\d+)\\.(\\d+) години?"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (h:m:_)):_) -> do
        hh <- parseInteger h
        mnum <- parseInteger m
        let mden = 10 ^ Text.length m
        Just . Token Duration $ minutesFromHourMixedFraction hh mnum mden
      _ -> Nothing
  }

ruleIntegerAndAnHalfHours :: Rule
ruleIntegerAndAnHalfHours = Rule
  { name = "<integer> and an half hours"
  , pattern =
    [ Predicate isNatural
    , regex "з половиною години"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v}:_) ->
        Just . Token Duration . duration TG.Minute $ 30 + 60 * floor v
      _ -> Nothing
  }

ruleAboutDuration :: Rule
ruleAboutDuration = Rule
  { name = "about <duration>"
  , pattern =
    [ regex "близько"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> Just token
      _ -> Nothing
  }

ruleExactlyDuration :: Rule
ruleExactlyDuration = Rule
  { name = "exactly <duration>"
  , pattern =
    [ regex "рівно"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> Just token
      _ -> Nothing
  }

ruleDurationHalfATimeGrain :: Rule
ruleDurationHalfATimeGrain = Rule
  { name = "half a <time-grain>"
  , pattern =
    [ regex "(1/2|пів)"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) -> Token Duration <$> timesOneAndAHalf grain 0
      _ -> Nothing
  }

ruleDurationOneGrainAndHalf :: Rule
ruleDurationOneGrainAndHalf = Rule
  { name = "one and a hald <unit-of-duration>"
  , pattern =
    [ regex "півтори"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) -> Token Duration <$> timesOneAndAHalf grain 1
      _ -> Nothing
  }

ruleCompositeDurationCommasAnd :: Rule
ruleCompositeDurationCommasAnd = Rule
  { name = "composite <duration> (with ,/and)"
  , pattern =
    [ Predicate isNatural
    , dimension TimeGrain
    , regex ",|і"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v}:
       Token TimeGrain g:
       _:
       Token Duration dd@DurationData{TDuration.grain = dg}:
       _) | g > dg -> Just . Token Duration $ duration g (floor v) <> dd
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleAboutDuration
  , ruleExactlyDuration
  , ruleHalfAnHour
  , ruleIntegerAndAnHalfHours
  , ruleNumeralnumberHours
  , ruleDurationHalfATimeGrain
  , ruleDurationOneGrainAndHalf
  , ruleCompositeDurationCommasAnd
  ]
