-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.HR.Rules
  ( rules ) where

import Control.Monad (join)
import Data.Maybe
import Data.String
import Data.Text (Text)
import Prelude
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Duration.Helpers
import Duckling.Duration.Types (DurationData (..))
import Duckling.Numeral.Helpers (parseInteger)
import Duckling.Numeral.Types (NumeralData(..))
import Duckling.Regex.Types
import Duckling.Types
import qualified Duckling.Duration.Types as TDuration
import qualified Duckling.Numeral.Types as TNumeral
import qualified Duckling.TimeGrain.Types as TG

ruleExactlyDuration :: Rule
ruleExactlyDuration = Rule
  { name = "exactly <duration>"
  , pattern =
    [ regex "to(c|č)no"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> Just token
      _ -> Nothing
  }

ruleIntegerAndAnHalfHours :: Rule
ruleIntegerAndAnHalfHours = Rule
  { name = "<integer> and an half hours"
  , pattern =
    [ Predicate isNatural
    , regex "i pol?a?"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v}:_) ->
        Just . Token Duration . duration TG.Minute $ 30 + 60 * floor v
      _ -> Nothing
  }

ruleIntegerMoreUnitofduration :: Rule
ruleIntegerMoreUnitofduration = Rule
  { name = "<integer> more <unit-of-duration>"
  , pattern =
    [ Predicate isNatural
    , regex "vi(s|š)e|manje"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v}:
       _:
       Token TimeGrain grain:_) ->
         Just . Token Duration . duration grain $ floor v
      _ -> Nothing
  }

ruleQuarterOfAnHour :: Rule
ruleQuarterOfAnHour = Rule
  { name = "quarter of an hour"
  , pattern =
    [ regex "((1/4|frtalj|kvarat|(c|č)etvrt)\\s?(h|sata)?)"
    ]
  , prod = \_ -> Just . Token Duration $ duration TG.Minute 15
  }

ruleAboutDuration :: Rule
ruleAboutDuration = Rule
  { name = "about <duration>"
  , pattern =
    [ regex "(oko|otprilike|odokativno)"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> Just token
      _ -> Nothing
  }

ruleNumbernumberHours :: Rule
ruleNumbernumberHours = Rule
  { name = "number.number hours"
  , pattern =
    [ regex "(\\d+)\\.(\\d+)"
    , regex "sat(i|a)?"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (h:m:_)):_) -> do
        hh <- parseInteger h
        mnum <- parseInteger m
        let mden = 10 ^ Text.length m
        Just . Token Duration $ minutesFromHourMixedFraction hh mnum mden
      _ -> Nothing
  }

ruleThreequartersOfAnHour :: Rule
ruleThreequartersOfAnHour = Rule
  { name = "three-quarters of an hour"
  , pattern =
    [ regex "((3/4|tri-?frtalja|tri-?kvarat|tri-?(c|č)etvrt(ine)?)\\s?(h|sata)?)"
    ]
  , prod = \_ -> Just . Token Duration $ duration TG.Minute 45
  }

ruleHalfAnHour :: Rule
ruleHalfAnHour = Rule
  { name = "half an hour"
  , pattern =
    [ regex "(1/2\\s?(h|sata)?|pol?a? sata)"
    ]
  , prod = \_ -> Just . Token Duration $ duration TG.Minute 30
  }

rules :: [Rule]
rules =
  [ ruleAboutDuration
  , ruleExactlyDuration
  , ruleHalfAnHour
  , ruleIntegerAndAnHalfHours
  , ruleIntegerMoreUnitofduration
  , ruleNumbernumberHours
  , ruleQuarterOfAnHour
  , ruleThreequartersOfAnHour
  ]
