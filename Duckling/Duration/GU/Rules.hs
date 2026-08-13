-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.GU.Rules
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
  [ ("મિનિટ"   , TG.Minute)
  , ("મિનિટની" , TG.Minute)
  , ("ક્ષણ"    , TG.Minute)
  , ("કલાક"   , TG.Hour)
  , ("કલાકનો"  , TG.Hour)
  , ("કલાકની"  , TG.Hour)
  , ("કલાકનું"  , TG.Hour)
  , ("કલાકના"  , TG.Hour)
  , ("દિવસ"   , TG.Day)
  , ("દિવસનો"  , TG.Day)
  , ("દિન"    , TG.Day)
  , ("મહિનો"  , TG.Month)
  , ("મહિના"  , TG.Month)
  , ("મહિનાનો" , TG.Month)
  , ("માસ"    , TG.Month)
  , ("વર્ષ"   , TG.Year)
  , ("વર્ષનો"  , TG.Year)
  , ("સાલ"    , TG.Year)
  ]

ruleAadha :: Rule
ruleAadha = Rule
  { name = "half of a duration"
  , pattern =
    [ regex "(અડધો|અડધી|અડધું|અડધા)\\s+((વર્ષનો|વર્ષ|સાલ)|(મહિનાનો|મહિનો|મહિના|માસ)|(દિવસનો|દિવસ|દિન)|(કલાકનો|કલાકની|કલાકનું|કલાકના|કલાક)|(મિનિટની|મિનિટ|ક્ષણ))"
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
    [ regex "પંદર મિનિટ"
    ]
  , prod = \_ -> Just $ Token Duration $ duration TG.Minute 15
  }

ruleDurationDin :: Rule
ruleDurationDin = Rule
  { name = "a day"
  , pattern =
    [ regex "((એક દિવસ)|(એક દિન)|(દિવસ|દિન))"
    ]
  , prod = \_ -> Just $ Token Duration $ duration TG.Day 1
  }

ruleDurationEkSaal :: Rule
ruleDurationEkSaal = Rule
  { name = "one year"
  , pattern =
    [ regex "એક (વર્ષ|સાલ)"
    ]
  , prod = \_ -> Just $ Token Duration $ duration TG.Year 1
  }

ruleDurationPreciseImprecise :: Rule
ruleDurationPreciseImprecise = Rule
  { name = "about|exactly <duration>"
  , pattern =
    [ regex "(લગભગ|બરાબર|માત્ર)"
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
  { name = "composite <duration> (with ,/અને)"
  , pattern =
    [ dimension Duration
    , regex ",|અને"
    , dimension Duration
    ]
  , prod = \case
      (Token Duration d1@DurationData{TDuration.grain = g1}:
       _:
       Token Duration d2@DurationData{TDuration.grain = g2}:
       _) | g1 > g2 -> Just $ Token Duration $ d1 <> d2
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleDurationPandrahMinat
  , ruleDurationDin
  , ruleAadha
  , ruleDurationEkSaal
  , ruleDurationPreciseImprecise
  , ruleCompositeDuration
  , ruleCompositeDurationDirect
  , ruleCompositeDurationCommasAnd
  , ruleIntegerUnitofduration
  ]
