-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.AR.Rules
  ( rules
  ) where

import Data.String
import Prelude
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Duration.Helpers
import Duckling.Numeral.Helpers (parseInt, parseInteger)
import Duckling.Numeral.Types (NumeralData(..))
import Duckling.Regex.Types
import Duckling.Types
import qualified Duckling.Numeral.Types as TNumeral
import qualified Duckling.TimeGrain.Types as TG

ruleDurationQuarterOfAnHour :: Rule
ruleDurationQuarterOfAnHour = Rule
  { name = "quarter of an hour"
  , pattern =
    [ regex "(ربع ساعة)"
    ]
  , prod = \_ -> Just . Token Duration $ duration TG.Minute 15
  }

ruleDurationHalfAnHour :: Rule
ruleDurationHalfAnHour = Rule
  { name = "half an hour"
  , pattern =
    [ regex "(1/2\\s?ساع[ةه]?|نصف? ساع[ةه])"
    ]
  , prod = \_ -> Just . Token Duration $ duration TG.Minute 30
  }

ruleDurationThreeQuartersOfAnHour :: Rule
ruleDurationThreeQuartersOfAnHour = Rule
  { name = "three-quarters of an hour"
  , pattern =
    [ regex "(3/4\\s?(ال)?ساع[ةه]?|ثلاث[ةه]?(\\s|-)[أا]رباع (ال)?ساع[ةه])"
    ]
  , prod = \_ -> Just . Token Duration $ duration TG.Minute 45
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
         "'"  -> Just . Token Duration . duration TG.Minute $ floor v
         "\"" -> Just . Token Duration . duration TG.Second $ floor v
         _    -> Nothing
      _ -> Nothing
  }

ruleDurationDotNumeralHours :: Rule
ruleDurationDotNumeralHours = Rule
  { name = "number.number hours"
  , pattern =
    [ regex "(\\d+)\\.(\\d+) *ساع(ة|ات)"
    ]
  , prod = \tokens -> case tokens of
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
    , regex "و ?نصف? ساع[ةه]"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v}:_) ->
        Just . Token Duration . duration TG.Minute $ 30 + 60 * floor v
      _ -> Nothing
  }

ruleTwoSeconds :: Rule
ruleTwoSeconds = Rule
  { name = "two seconds"
  , pattern =
    [ regex "ثانيتين|ثانيتان|لحظتين|لحظتان"
    ]
  , prod = \_ -> Just . Token Duration $ duration TG.Second 2
  }

ruleTwoMinutes :: Rule
ruleTwoMinutes = Rule
  { name = "two minutes"
  , pattern =
    [ regex "دقيقتين|دقيقتان"
    ]
  , prod = \_ -> Just . Token Duration $ duration TG.Minute 2
  }

ruleTwoHours :: Rule
ruleTwoHours = Rule
  { name = "two hours"
  , pattern =
    [ regex "ساعتين|ساعتان"
    ]
  , prod = \_ -> Just . Token Duration $ duration TG.Hour 2
  }

ruleTwoYears :: Rule
ruleTwoYears = Rule
  { name = "dual years"
  , pattern =
    [ regex "سنتين|سنتان"
    ]
  , prod = \_ -> Just . Token Duration $ duration TG.Year 2
  }

-- this rule handles TG.Day, TG.Week and TG.Month
ruleDualUnitofduration :: Rule
ruleDualUnitofduration = Rule
  { name = "dual <unit-of-duration>"
  , pattern =
    [ dimension TimeGrain
    , regex "(ان|ين)"
    ]
  , prod = \tokens -> case tokens of
      (Token TimeGrain grain:_) -> Just . Token Duration $ duration grain 2
      _ -> Nothing
  }

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
  [ ruleDurationQuarterOfAnHour
  , ruleDurationHalfAnHour
  , ruleDurationThreeQuartersOfAnHour
  , ruleDurationDotNumeralHours
  , ruleDurationAndHalfHour
  , ruleNumeralQuotes
  , ruleTwoSeconds
  , ruleTwoMinutes
  , ruleTwoHours
  , ruleTwoYears
  , ruleDualUnitofduration
  , ruleSingleUnitofduration
  ]
