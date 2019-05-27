-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.KO.Rules
  ( rules ) where

import Control.Monad (join)
import qualified Data.Text as Text
import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Duration.Helpers
import Duckling.Numeral.Helpers (parseInteger)
import Duckling.Numeral.Types (NumeralData (..))
import qualified Duckling.Numeral.Types as TNumeral
import Duckling.Regex.Types
import qualified Duckling.TimeGrain.Types as TG
import Duckling.Types

ruleHalfAnHour :: Rule
ruleHalfAnHour = Rule
  { name = "half an hour"
  , pattern =
    [ Predicate $ isGrain TG.Hour
    , regex "반"
    ]
  , prod = \_ -> Just . Token Duration $ duration TG.Minute 30
  }

ruleADay :: Rule
ruleADay = Rule
  { name = "a day - 하루"
  , pattern =
    [ regex "하루"
    ]
  , prod = \_ -> Just . Token Duration $ duration TG.Day 1
  }

ruleNumeralnumberHours :: Rule
ruleNumeralnumberHours = Rule
  { name = "number.number hours"
  , pattern =
    [ regex "(\\d+)\\.(\\d+)"
    , regex "시간"
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
    , regex "시간반"
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
    [ regex "대충|약"
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
    [ regex "정확히"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> Just token
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleADay
  , ruleAboutDuration
  , ruleExactlyDuration
  , ruleHalfAnHour
  , ruleIntegerAndAnHalfHours
  , ruleNumeralnumberHours
  ]
