-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.DE.Rules
  ( rules ) where


import Duckling.Dimensions.Types
import Duckling.Duration.Helpers
import Duckling.Regex.Types
import Control.Monad (join)
import qualified Data.Text as Text
import Prelude
import Duckling.Numeral.Helpers (parseInteger)
import Duckling.Duration.Types (DurationData(..))
import qualified Duckling.Duration.Types as TDuration
import Data.String
import Duckling.Numeral.Types (NumeralData(..))
import qualified Duckling.TimeGrain.Types as TG
import qualified Duckling.Numeral.Types as TNumeral
import Duckling.Types

ruleQuarterOfAnHour :: Rule
ruleQuarterOfAnHour = Rule
  { name = "quarter of an hour"
  , pattern =
    [ regex "(einer? )?Viertelstunde"
    ]
  , prod = \_ -> Just $ Token Duration $ duration TG.Minute 15
  }

ruleHalfAnHour :: Rule
ruleHalfAnHour = Rule
  { name = "half an hour"
  , pattern =
    [ regex "(1/2\\s?|(eine )?halbe |(einer )?halben )stunde"
    ]
  , prod = \_ -> Just $ Token Duration $ duration TG.Minute 30
  }

ruleThreeQuartersOfAnHour :: Rule
ruleThreeQuartersOfAnHour = Rule
  { name = "three-quarters of an hour"
  , pattern =
    [ regex "3/4\\s?stunde|(einer? )?dreiviertel stunde|drei viertelstunden"
    ]
  , prod = \_ -> Just $ Token Duration $ duration TG.Minute 45
  }

ruleFortnight :: Rule
ruleFortnight = Rule
  { name = "fortnight"
  , pattern =
    [ regex "zwei Wochen"
    ]
  , prod = \_ -> Just $ Token Duration $ duration TG.Day 14
  }

rulePrecision :: Rule
rulePrecision = Rule
  { name = "about|exactly <duration>"
  , pattern =
    [ regex "ungef(ä|a)hr|zirka|genau|exakt"
    , dimension Duration
    ]
  , prod = \case
      (_:token:_) -> Just token
      _ -> Nothing
  }

ruleCommaNumeralHours :: Rule
ruleCommaNumeralHours = Rule
  { name = "number,number hours"
  , pattern =
    [ regex "(\\d+),(\\d+)"
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

ruleCommaNumeralMinutes :: Rule
ruleCommaNumeralMinutes = Rule
  { name = "number,number minutes"
  , pattern =
    [ regex "(\\d+),(\\d+)"
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

ruleAndHalfHour :: Rule
ruleAndHalfHour = Rule
  { name = "<integer> and a half hour"
  , pattern =
    [ Predicate isNatural
    , regex "(und )?(ein(en?)? )?halb(en?)?"
    , Predicate $ isGrain TG.Hour
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = v}:_) ->
        Just $ Token Duration $ duration TG.Minute $ 30 + 60 * floor v
      _ -> Nothing
  }

ruleAndHalfMinute :: Rule
ruleAndHalfMinute = Rule
  { name = "<integer> and a half minutes"
  , pattern =
    [ Predicate isNatural
    , regex "(und )?(ein(en?)? ?)?halb(en?)?"
    , Predicate $ isGrain TG.Minute
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = v}:_) ->
        Just $ Token Duration $ duration TG.Second $ 30 + 60 * floor v
      _ -> Nothing
  }

ruleArticle :: Rule
ruleArticle = Rule
  { name = "a <unit-of-duration>"
  , pattern =
    [ regex "ein(en?)?"
    , dimension TimeGrain
    ]
  , prod = \case
      (_:Token TimeGrain grain:_) -> Just $ Token Duration $ duration grain 1
      _ -> Nothing
  }

ruleHalfTimeGrain :: Rule
ruleHalfTimeGrain = Rule
  { name = "half a <time-grain>"
  , pattern =
    [ regex "(ein(en)?)?(1/2|halbe?)"
    , dimension TimeGrain
    ]
  , prod = \case
      (_:Token TimeGrain grain:_) -> Token Duration <$> nPlusOneHalf grain 0
      _ -> Nothing
  }

ruleCompositeDurationCommasAnd :: Rule
ruleCompositeDurationCommasAnd = Rule
  { name = "composite <duration> (with ,/and)"
  , pattern =
    [ Predicate isNatural
    , dimension TimeGrain
    , regex ",|und"
    , dimension Duration
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = v}:
       Token TimeGrain g:
       _:
       Token Duration dd@DurationData{TDuration.grain = dg}:
       _) | g > dg -> Just $ Token Duration $ duration g (floor v) <> dd
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
       _) | g > dg -> Just $ Token Duration $ duration g (floor v) <> dd
      _ -> Nothing
  }

ruleCompositeDurationAnd :: Rule
ruleCompositeDurationAnd = Rule
  { name = "composite <duration> and <duration>"
  , pattern =
    [ dimension Duration
    , regex ",|und"
    , dimension Duration
    ]
  , prod = \case
      (Token Duration DurationData{TDuration.value = v, TDuration.grain = g}:
       _:
       Token Duration dd@DurationData{TDuration.grain = dg}:
       _) | g > dg -> Just $ Token Duration $ duration g v <> dd
      _ -> Nothing
  }

ruleHoursAndMinutes :: Rule
ruleHoursAndMinutes = Rule
  { name = "<integer> hour and <integer>"
  , pattern =
    [ Predicate isNatural
    , regex "(ein(en?) )?stunden?( und)?"
    , Predicate isNatural
    , Predicate $ isGrain TG.Minute
    ]
  , prod = \case
      (Token Numeral h:
       _:
       Token Numeral m:
       _) -> Just $ Token Duration $ duration TG.Minute $
                floor (TNumeral.value m) + 60 * floor (TNumeral.value h)
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleQuarterOfAnHour
  , ruleHalfAnHour
  , ruleThreeQuartersOfAnHour
  , rulePrecision
  , ruleCommaNumeralHours
  , ruleCommaNumeralMinutes
  , ruleFortnight
  , ruleAndHalfHour
  , ruleAndHalfMinute
  , ruleArticle
  , ruleHalfTimeGrain
  , ruleCompositeDurationCommasAnd
  , ruleCompositeDuration
  , ruleCompositeDurationAnd
  , ruleHoursAndMinutes
  , ruleAndHalfMinute
  ]
