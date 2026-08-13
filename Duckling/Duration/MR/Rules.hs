-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.MR.Rules
  ( rules
  ) where

import Data.String
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Prelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Duration.Helpers
import Duckling.Duration.Rules (ruleIntegerUnitofduration)
import Duckling.Duration.Types (DurationData(..))
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Regex.Types
import Duckling.Types
import qualified Duckling.Duration.Types as TDuration
import qualified Duckling.Numeral.Types as TNumeral
import qualified Duckling.TimeGrain.Types as TG

avadhiMap :: HashMap Text TG.Grain
avadhiMap = HashMap.fromList
  [ ("मिनिट"   , TG.Minute)
  , ("मिनिटे"  , TG.Minute)
  , ("मिनिटांचा", TG.Minute)
  , ("मिनिटांची", TG.Minute)
  , ("मिनिटांचे", TG.Minute)
  , ("तास"    , TG.Hour)
  , ("तासाचा"  , TG.Hour)
  , ("तासाची"  , TG.Hour)
  , ("तासाचे"  , TG.Hour)
  , ("तासांचा" , TG.Hour)
  , ("दिवस"   , TG.Day)
  , ("दिवसाचा" , TG.Day)
  , ("महिना"  , TG.Month)
  , ("महिन्याचा", TG.Month)
  , ("मास"    , TG.Month)
  , ("वर्ष"   , TG.Year)
  , ("वर्षाचा"  , TG.Year)
  , ("साल"    , TG.Year)
  ]

ruleAadha :: Rule
ruleAadha = Rule
  { name = "half of a duration"
  , pattern =
    [ regex "(अर्धा|अर्धी|अर्धे|अर्ध्या)\\s+((वर्षाचा|वर्ष|साल)|(महिन्याचा|महिना|मास)|(दिवसाचा|दिवस)|(तासाचा|तासाची|तासाचे|तासांचा|तास)|(मिनिटांचा|मिनिट|मिनिटे))"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (_:x:_)):_) -> do
        grain <- HashMap.lookup (Text.toLower x) avadhiMap
        Token Duration <$> nPlusOneHalf grain 0
      _ -> Nothing
  }

ruleDurationPandrahMinat :: Rule
ruleDurationPandrahMinat = Rule
  { name = "quarter of an hour"
  , pattern =
    [ regex "पंधरा मिनिटे"
    ]
  , prod = \_ -> Just $ Token Duration $ duration TG.Minute 15
  }

ruleDurationDin :: Rule
ruleDurationDin = Rule
  { name = "a day"
  , pattern =
    [ regex "(एक दिवस|दिवसभर)"
    ]
  , prod = \_ -> Just $ Token Duration $ duration TG.Day 1
  }

ruleDurationEkSaal :: Rule
ruleDurationEkSaal = Rule
  { name = "one year"
  , pattern =
    [ regex "एक (वर्ष|साल)"
    ]
  , prod = \_ -> Just $ Token Duration $ duration TG.Year 1
  }

ruleDurationPreciseImprecise :: Rule
ruleDurationPreciseImprecise = Rule
  { name = "about|exactly <duration>"
  , pattern =
    [ regex "(सुमारे|अंदाजे|नेमके|फक्त)"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> Just token
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

ruleCompositeDurationDirect :: Rule
ruleCompositeDurationDirect = Rule
  { name = "<duration> <duration>"
  , pattern =
    [ dimension Duration
    , dimension Duration
    ]
  , prod = \case
      (Token Duration d1@DurationData{TDuration.grain = g1}:
       Token Duration d2@DurationData{TDuration.grain = g2}:
       _) | g1 > g2 -> Just $ Token Duration $ d1 <> d2
      _ -> Nothing
  }

ruleCompositeDurationCommasAnd :: Rule
ruleCompositeDurationCommasAnd = Rule
  { name = "composite <duration> (with ,/आणि)"
  , pattern =
    [ dimension Duration
    , regex ",|आणि"
    , dimension Duration
    ]
  , prod = \case
      (Token Duration d1@DurationData{TDuration.grain = g1}:
       _:
       Token Duration d2@DurationData{TDuration.grain = g2}:
       _) | g1 > g2 -> Just $ Token Duration $ d1 <> d2
      _ -> Nothing
  }

ruleIntegerChiDuration :: Rule
ruleIntegerChiDuration = Rule
  { name = "<integer> ची|चा|चे (duration in minutes)"
  , pattern =
    [ Predicate isNatural
    , regex "(ची|चा|चे)"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v}:_) ->
        Just . Token Duration . duration TG.Minute $ floor v
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleIntegerUnitofduration
  , ruleIntegerChiDuration
  , ruleDurationPandrahMinat
  , ruleDurationDin
  , ruleAadha
  , ruleDurationEkSaal
  , ruleDurationPreciseImprecise
  , ruleCompositeDuration
  , ruleCompositeDurationDirect
  , ruleCompositeDurationCommasAnd
  ]
