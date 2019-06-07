-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.MN.Rules
  ( rules
  ) where

import Data.HashMap.Strict (HashMap)
import Data.String
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
import qualified Duckling.Numeral.Types as TNumeral

grainsMap :: HashMap Text Grain
grainsMap = HashMap.fromList
  [ ("жил"  , Year)
  , ("сар", Month)
  , ("өдөр"   , Day)
  , ("цаг"  , Hour)
  , ("минут", Minute)
  ]

-- TODO: Single-word composition (#110)
ruleHalves :: Rule
ruleHalves = Rule
  { name = "half of a grain"
  , pattern =
    [ regex "хагас\\s?(жил|сар|өдөр|цаг|минут)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (x:_)):_) -> do
        grain <- HashMap.lookup (Text.toLower x) grainsMap
        Token Duration <$> timesOneAndAHalf grain 0
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
    [ regex "(ойролцоогоор|яг)"
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
  [ rulePositiveDuration
  , ruleDurationPrecision
  , ruleNumeralQuotes
  , ruleGrainAsDuration
  , ruleHalves
  ]
