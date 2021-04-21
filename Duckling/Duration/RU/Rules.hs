-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.RU.Rules
  ( rules
  ) where

import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Prelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Duration.Helpers
import Duckling.Numeral.Helpers (numberWith)
import Duckling.Numeral.Types (NumeralData(..), isInteger)
import Duckling.Duration.Types (DurationData (DurationData))
import Duckling.Regex.Types
import Duckling.Types
import Duckling.TimeGrain.Types
import qualified Duckling.Duration.Types as TDuration
import qualified Duckling.Numeral.Types as TNumeral
import qualified Duckling.TimeGrain.Types as TG

grainsMap :: HashMap Text Grain
grainsMap = HashMap.fromList
  [ ("года"  , Year)
  , ("месяца", Month)
  , ("дня"   , Day)
  , ("часа"  , Hour)
  , ("минуты", Minute)
  ]

-- TODO: Single-word composition (#110)
ruleHalves :: Rule
ruleHalves = Rule
  { name = "half of a grain"
  , pattern =
    [ regex "пол\\s?(года|месяца|дня|часа|минуты)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (x:_)):_) -> do
        grain <- HashMap.lookup (Text.toLower x) grainsMap
        Token Duration <$> nPlusOneHalf grain 0
      _ -> Nothing
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
         "'"  -> Just $ Token Duration $ duration Minute $ floor v
         "\"" -> Just $ Token Duration $ duration Second $ floor v
         _    -> Nothing
      _ -> Nothing
  }

ruleDurationPrecision :: Rule
ruleDurationPrecision = Rule
  { name = "about|exactly <duration>"
  , pattern =
    [ regex "(где-то|приблизительно|примерно|ровно)"
    , dimension Duration
    ]
    , prod = \case
        (_:token:_) -> Just token
        _ -> Nothing
  }

ruleGrainAsDuration :: Rule
ruleGrainAsDuration = Rule
  { name = "a <unit-of-duration>"
  , pattern =
    [ dimension TimeGrain
    ]
  , prod = \case
      (Token TimeGrain grain:_) -> Just $ Token Duration $ duration grain 1
      _ -> Nothing
  }

rulePositiveDuration :: Rule
rulePositiveDuration = Rule
  { name = "<positive-non-integer> <time-grain>"
  , pattern =
    [ numberWith TNumeral.value $ and . sequence [not . isInteger, (>0)]
    , dimension TimeGrain
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = v}:
       Token TimeGrain grain:
       _) ->
        Just $ Token Duration $ inCoarsestGrain grain v
      _ -> Nothing
  }

hourDiminutive :: Rule
hourDiminutive = Rule
  { name = "hour diminutive"
  , pattern =
    [ regex "час(ок|ик|очек)"
    ]
  , prod = \case
      _ -> Just $ Token Duration $ duration Hour 1
  }

hoursDiminutive :: Rule
hoursDiminutive = Rule
  { name = "hour diminutive 2"
  , pattern =
    [ numberWith TNumeral.value isInteger
    , regex "час(иков|очков)"
    ]
  , prod = \case
      Token Numeral NumeralData{TNumeral.value = v}:_ -> do
        n <- TNumeral.getIntValue v
        Just $ Token Duration $ duration Hour n
      _ -> Nothing
  }

minuteDiminutive :: Rule
minuteDiminutive = Rule
  { name = "minute diminutive"
  , pattern =
    [ regex "минутк.|минуточк."
    ]
  , prod = \case
      _ -> Just $ Token Duration $ duration Minute 1
  }

minutesDiminutive :: Rule
minutesDiminutive = Rule
  { name = "minute diminutive"
  , pattern =
    [ numberWith TNumeral.value isInteger
    , regex "минутк.|минуток|минуточк.|минуточек"
    ]
  , prod = \case
      Token Numeral NumeralData{TNumeral.value = v}:_ -> do
        n <- TNumeral.getIntValue v
        Just $ Token Duration $ duration Minute n
      _ -> Nothing
  }

ruleDurationQuarterOfAnHour :: Rule
ruleDurationQuarterOfAnHour = Rule
  { name = "quarter of an hour"
  , pattern =
    [ regex "((одн(у|а|ой)|1)\\s)?четверт. (часа|ч|ч\\.)"
    ]
  , prod = \_ -> Just $ Token Duration $ duration TG.Minute 15
  }

ruleDurationThreeQuartersOfAnHour :: Rule
ruleDurationThreeQuartersOfAnHour = Rule
  { name = "3 quarters of an hour"
  , pattern =
    [ numberWith TNumeral.value (== 3)
    , regex "четверт(и|ей) (часа|ч|ч\\.)"
    ]
  , prod = \_ -> Just $ Token Duration $ duration TG.Minute 45
  }

ruleDuration24h :: Rule
ruleDuration24h = Rule
  { name = "сутки"
  , pattern =
    [ regex "сутки"
    ]
  , prod = \_ -> Just $ Token Duration $ duration TG.Hour 24
  }

ruleDuration24hN :: Rule
ruleDuration24hN = Rule
  { name = "<integer> сутки"
  , pattern =
    [ numberWith TNumeral.value isInteger
    , regex "сутки|суток"
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = v}:_) -> do
        n <- TNumeral.getIntValue v
        Just $ Token Duration $ duration TG.Hour (n * 24)
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

rules :: [Rule]
rules =
  [ rulePositiveDuration
  , ruleDurationPrecision
  , ruleNumeralQuotes
  , ruleGrainAsDuration
  , ruleHalves
  , hourDiminutive
  , hoursDiminutive
  , minuteDiminutive
  , minutesDiminutive
  , ruleDurationQuarterOfAnHour
  , ruleDurationThreeQuartersOfAnHour
  , ruleDuration24h
  , ruleDuration24hN
  , ruleCompositeDuration
  ]
