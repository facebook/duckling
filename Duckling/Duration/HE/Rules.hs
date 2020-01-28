-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.HE.Rules
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

ruleQuarterOfAnHour :: Rule
ruleQuarterOfAnHour = Rule
  { name = "quarter of an hour"
  , pattern =
    [ regex "(1/4/s שעה|רבע שעה)"
    ]
  , prod = \_ -> Just . Token Duration $ duration TG.Minute 15
  }

ruleHalfAnHour :: Rule
ruleHalfAnHour = Rule
  { name = "half an hour"
  , pattern =
    [ regex "(1/2/s שעה|חצי שעה)"
    ]
  , prod = \_ -> Just . Token Duration $ duration TG.Minute 30
  }

ruleThreequartersOfAnHour :: Rule
ruleThreequartersOfAnHour = Rule
  { name = "three-quarters of an hour"
  , pattern =
    [ regex "(3/4/s שעה|שלושת רבעי שעה)"
    ]
  , prod = \_ -> Just . Token Duration $ duration TG.Minute 45
  }

ruleNumbernumberHours :: Rule
ruleNumbernumberHours = Rule
  { name = "number.number hours"
  , pattern =
    [ regex "(\\d+)\\.(\\d+)"
    , regex "שעה|שעות"
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
    , regex "וחצי (שעות|שעה)"
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
    [ regex "(בערך|סביבות|בקירוב)"
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
    [ regex "בדיוק"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> Just token
      _ -> Nothing
  }
  
  
ruleHalfAYear :: Rule
ruleHalfAYear = Rule
  { name = "half a year"
  , pattern =
    [ regex "(1/2 שנה|חצי שנה)"
    ]
  , prod = \_ -> Just . Token Duration $ duration TG.Month 6
  } 
  
-- this rule handles TG.Week and TG.Month
ruleDualUnitofduration :: Rule
ruleDualUnitofduration = Rule
  { name = "dual <unit-of-duration>"
  , pattern =
    [ dimension TimeGrain
    , regex "(יים)"
    ]
  , prod = \tokens -> case tokens of
      (Token TimeGrain grain:_) -> Just . Token Duration $ duration grain 2
      _ -> Nothing
  }
  
ruleTwoDays :: Rule
ruleTwoDays = Rule
  { name = "dual days"
  , pattern =
    [ regex "יומיים"
    ]
  , prod = \_ -> Just . Token Duration $ duration TG.Day 2
  }

ruleTwoYears :: Rule
ruleTwoYears = Rule
  { name = "dual years"
  , pattern =
    [ regex "שנתיים"
    ]
  , prod = \_ -> Just . Token Duration $ duration TG.Year 2
  }
  
--There's no word in Hebrew for "a" (and no need), so just the TimeGrain represents a duration
ruleSingleUnitofduration :: Rule
ruleSingleUnitofduration = Rule
  { name = "single <unit-of-duration>"
  , pattern =
    [ dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (Token TimeGrain grain:_) -> Just . Token Duration $ duration grain 1
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleDualUnitofduration
  , ruleTwoDays
  , ruleTwoYears
  , ruleHalfAYear
  , ruleAboutDuration
  , ruleExactlyDuration
  , ruleHalfAnHour
  , ruleIntegerAndAnHalfHours
  , ruleNumbernumberHours
  , ruleQuarterOfAnHour
  , ruleThreequartersOfAnHour
  , ruleSingleUnitofduration
  ]
