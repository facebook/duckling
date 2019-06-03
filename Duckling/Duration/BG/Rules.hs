-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.BG.Rules
  ( rules
  ) where

import Data.String
import Data.Text (Text)
import Prelude
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Duration.Helpers
import Duckling.Numeral.Helpers (numberWith)
import Duckling.Numeral.Types (NumeralData(..), isInteger)
import Duckling.Duration.Types (DurationData (DurationData))
import Duckling.Regex.Types
import Duckling.Types
import Duckling.TimeGrain.Types
import qualified Duckling.Numeral.Types as TNumeral

ruleHalves :: Rule
ruleHalves = Rule
  { name = "half of a <time-grain>"
  , pattern =
    [ regex "половин"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) -> Token Duration <$> timesOneAndAHalf grain 0
      _ -> Nothing
  }

ruleGrainAndAHalf :: Rule
ruleGrainAndAHalf = Rule
  { name = "<time-grain> and a half"
  , pattern =
    [ dimension TimeGrain
    , regex "и половина"
    ]
  , prod = \tokens -> case tokens of
      (Token TimeGrain grain:_) -> Token Duration <$> timesOneAndAHalf grain 1
      _ -> Nothing
  }

ruleDurationAndAHalf :: Rule
ruleDurationAndAHalf = Rule
  { name = "<positive-numeral> <time-grain> and a half"
  , pattern =
    [ Predicate isNatural
    , dimension TimeGrain
    , regex "и половина"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v}:
       Token TimeGrain grain:
       _) -> timesOneAndAHalf grain (floor $ v) >>= Just . Token Duration
      _ -> Nothing
  }

ruleNumeralQuotes :: Rule
ruleNumeralQuotes = Rule
  { name = "<integer> + '\""
  , pattern =
    [ Predicate isNatural
    , regex "(['\"])"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v}:
       Token RegexMatch (GroupMatch (x:_)):
       _) -> case x of
         "'"  -> Just . Token Duration . duration Minute $ floor v
         "\"" -> Just . Token Duration . duration Second $ floor v
         _    -> Nothing
      _ -> Nothing
  }

ruleDurationPrecision :: Rule
ruleDurationPrecision = Rule
  { name = "about|exactly <duration>"
  , pattern =
    [ regex "(към|приблизително|примерно|някъде)"
    , dimension Duration
    ]
    , prod = \tokens -> case tokens of
        (_:token:_) -> Just token
        _ -> Nothing
  }

ruleGrainAsDuration :: Rule
ruleGrainAsDuration = Rule
  { name = "a <unit-of-duration>"
  , pattern =
    [ dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (Token TimeGrain grain:_) -> Just . Token Duration $ duration grain 1
      _ -> Nothing
  }

rulePositiveDuration :: Rule
rulePositiveDuration = Rule
  { name = "<positive-numeral> <time-grain>"
  , pattern =
    [ numberWith TNumeral.value $ and . sequence [not . isInteger, (>0)]
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v}:
       Token TimeGrain grain:
       _) -> Just . Token Duration . duration Second . floor $ inSeconds grain v
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleDurationAndAHalf
  , ruleGrainAndAHalf
  , rulePositiveDuration
  , ruleDurationPrecision
  , ruleNumeralQuotes
  , ruleGrainAsDuration
  , ruleHalves
  ]
