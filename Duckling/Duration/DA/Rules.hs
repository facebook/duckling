-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.DA.Rules
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

ruleExactlyDuration :: Rule
ruleExactlyDuration = Rule
  { name = "exactly <duration>"
  , pattern =
    [ regex "pr(æ)cis"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      -- TODO(jodent) +precision exact
      (_:token:_) -> Just token
      _ -> Nothing
  }

ruleIntegerAndAnHalfHours :: Rule
ruleIntegerAndAnHalfHours = Rule
  { name = "<integer> and an half hours"
  , pattern =
    [ Predicate isNatural
    , regex "og (en )?halv timer?"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v}:_) ->
        Just . Token Duration . duration TG.Minute $ 30 + 60 * floor v
      _ -> Nothing
  }

ruleAUnitofduration :: Rule
ruleAUnitofduration = Rule
  { name = "a <unit-of-duration>"
  , pattern =
    [ regex "en|et?"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) -> Just . Token Duration $ duration grain 1
      _ -> Nothing
  }

ruleIntegerMoreUnitofduration :: Rule
ruleIntegerMoreUnitofduration = Rule
  { name = "<integer> more <unit-of-duration>"
  , pattern =
    [ Predicate isNatural
    , dimension TimeGrain
    , regex "mere|mindre"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v}:
       Token TimeGrain grain:
       _) -> Just . Token Duration . duration grain $ floor v
      _ -> Nothing
  }

ruleFortnight :: Rule
ruleFortnight = Rule
  { name = "fortnight"
  , pattern =
    [ regex "(a|one)? fortnight"
    ]
  , prod = \_ -> Just . Token Duration $ duration TG.Day 14
  }

ruleAboutDuration :: Rule
ruleAboutDuration = Rule
  { name = "about <duration>"
  , pattern =
    [ regex "(omkring|cirka|ca.)"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      -- TODO(jodent) +precision approximate
      (_:token:_) -> Just token
      _ -> Nothing
  }

ruleNumeralnumberHours :: Rule
ruleNumeralnumberHours = Rule
  { name = "number.number hours"
  , pattern =
    [ regex "(\\d+)\\,(\\d+)"
    , regex "timer?"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (h:m:_)):_) -> do
        hh <- parseInteger h
        mnum <- parseInteger m
        let mden = 10 ^ Text.length m
        Just . Token Duration $ minutesFromHourMixedFraction hh mnum mden
      _ -> Nothing
  }

ruleHalfAnHour :: Rule
ruleHalfAnHour = Rule
  { name = "half an hour"
  , pattern =
    [ regex "(1/2|en halv) time"
    ]
  , prod = \_ -> Just . Token Duration $ duration TG.Minute 30
  }

rules :: [Rule]
rules =
  [ ruleAUnitofduration
  , ruleAboutDuration
  , ruleExactlyDuration
  , ruleFortnight
  , ruleHalfAnHour
  , ruleIntegerAndAnHalfHours
  , ruleIntegerMoreUnitofduration
  , ruleNumeralnumberHours
  ]
