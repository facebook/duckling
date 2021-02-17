-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.HI.Rules
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
import Duckling.Duration.Types (DurationData(..))
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Regex.Types
import Duckling.Types
import qualified Duckling.Duration.Types as TDuration
import qualified Duckling.Numeral.Types as TNumeral
import qualified Duckling.TimeGrain.Types as TG

avadhiMap :: HashMap Text TG.Grain
avadhiMap = HashMap.fromList
  [ ("मिनट" , TG.Minute)
  , ("क्षण", TG.Minute)
  , ("घंटा" , TG.Hour)
  , ("दिवस" , TG.Day)
  , ("दिन"  , TG.Day)
  , ("महीना", TG.Month)
  , ("माह", TG.Month)
  , ("मास", TG.Month)
  , ("वर्ष" , TG.Year)
  , ("साल"  , TG.Year)
  , ("बरस", TG.Year)
  ]

ruleAadha :: Rule
ruleAadha = Rule
  { name = "half of a duration"
  , pattern =
    [ regex "आधा ((साल|वर्ष|बरस)|(महीना|मास|माह)|(दिन|दिवस)|(घंटा)|(मिनट|क्षण))"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (x:_)):_) -> do
        grain <- HashMap.lookup (Text.toLower x) avadhiMap
        Token Duration <$> nPlusOneHalf grain 0
      _ -> Nothing
  }

ruleDurationPandrahMinat :: Rule
ruleDurationPandrahMinat = Rule
  { name = "quarter of an hour"
  , pattern =
    [ regex "पंद्रह मिनट"
    ]
  , prod = \_ -> Just $ Token Duration $ duration TG.Minute 15
  }

ruleDurationPakhwaada :: Rule
ruleDurationPakhwaada = Rule
  { name = "fortnight"
  , pattern =
    [ regex "((एक पखवाड़ा)|(पखवाड़ा))"
    ]
  , prod = \_ -> Just $ Token Duration $ duration TG.Day 14
  }

ruleDurationDin :: Rule
ruleDurationDin = Rule
  { name = "a day"
  , pattern =
    [ regex "((एक दिन)|(एक दिवस)|(दिन|दिवस))"
    ]
  , prod = \_ -> Just $ Token Duration $ duration TG.Day 1
  }

ruleDurationEkSaal :: Rule
ruleDurationEkSaal = Rule
  { name = "one year"
  , pattern =
    [ regex "एक (साल|वर्ष|बरस)"
    ]
  , prod = \_ -> Just $ Token Duration $ duration TG.Year 1
  }

ruleDurationPreciseImprecise :: Rule
ruleDurationPreciseImprecise = Rule
  { name = "about|exactly <duration>"
  , pattern =
    [ regex "(लगभग|बिल्कुल|केवल)"
    , dimension Duration
    ]
    , prod = \tokens -> case tokens of
        (_:token:_) -> Just token
        _ -> Nothing
  }

rulePauneDuration :: Rule
rulePauneDuration = Rule
  { name = "Paune duration"
  , pattern =
    [ regex "पौने"
    , Predicate isNatural
    , dimension TimeGrain
    ]
  , prod = \case
      (_:
       Token Numeral NumeralData{TNumeral.value = v}:
       Token TimeGrain grain:
       _) -> Token Duration <$> timesThreeQuarter grain (floor (v - 1))
      _ -> Nothing
  }

ruleSavaDuration :: Rule
ruleSavaDuration = Rule
  { name = "Sava duration"
  , pattern =
    [ regex "सवा"
    , Predicate isNatural
    , dimension TimeGrain
    ]
  , prod = \case
      (_:
       Token Numeral NumeralData{TNumeral.value = v}:
       Token TimeGrain grain:
       _) -> Token Duration <$> timesOneQuarter grain (floor v)
      _ -> Nothing
  }

ruleSaadeDuration :: Rule
ruleSaadeDuration = Rule
  { name = "Saade duration"
  , pattern =
    [ regex "साढ़े|साड़े"
    , Predicate isNatural
    , dimension TimeGrain
    ]
  , prod = \case
      (_:
        Token Numeral NumeralData{TNumeral.value = v}:
        Token TimeGrain grain:
        _) -> Token Duration <$> nPlusOneHalf grain (floor v)
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

ruleCompositeDurationCommasAnd :: Rule
ruleCompositeDurationCommasAnd = Rule
  { name = "composite <duration> (with ,/और)"
  , pattern =
    [ Predicate isNatural
    , dimension TimeGrain
    , regex ",|और"
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

ruleNAndHalfDuration :: Rule
ruleNAndHalfDuration = Rule
  { name = "डेढ़|ढाई <duration>"
  , pattern =
    [ regex "(डेढ़|ढाई)"
    , dimension TimeGrain
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):
       Token TimeGrain grain:
       _) -> do
        h <- case Text.toLower match of
          "डेढ़" -> Just 1
          "ढाई" -> Just 2
          _ -> Nothing
        Token Duration <$> nPlusOneHalf grain h
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [  ruleDurationPandrahMinat
  ,  ruleDurationPakhwaada
  ,  ruleDurationDin
  ,  ruleAadha
  ,  ruleDurationEkSaal
  ,  ruleDurationPreciseImprecise
  ,  rulePauneDuration
  ,  ruleSavaDuration
  ,  ruleSaadeDuration
  ,  ruleCompositeDuration
  ,  ruleCompositeDurationCommasAnd
  ,  ruleNAndHalfDuration
  ]
