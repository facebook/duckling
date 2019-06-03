-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.KA.Rules
  ( rules
  ) where

import Data.Semigroup ((<>))
import Data.String
import Prelude
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Duration.Helpers
import Duckling.Duration.Types (DurationData(..))
import Duckling.Numeral.Helpers (parseInt, parseInteger)
import Duckling.Numeral.Types (NumeralData(..))
import Duckling.Regex.Types
import Duckling.Types
import qualified Duckling.Duration.Types as TDuration
import qualified Duckling.Numeral.Types as TNumeral
import qualified Duckling.TimeGrain.Types as TG

ruleCompositeDuration :: Rule
ruleCompositeDuration = Rule
  { name = "composite <duration>"
  , pattern =
    [ Predicate isNatural
    , dimension TimeGrain
    , regex ",|და"
    , dimension Duration
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = v}:
       Token TimeGrain g:_:
       Token Duration dd@DurationData{TDuration.grain = dg}:
       _) | g > dg -> Just . Token Duration $ duration g (floor v) <> dd
      _ -> Nothing
  }

ruleCompositeDuration1 :: Rule
ruleCompositeDuration1 = Rule
  { name = "composite <duration>"
  , pattern =
    [ dimension TimeGrain
    , regex ",|და"
    , dimension Duration
    ]
  , prod = \case
      (Token TimeGrain g:_:
       Token Duration dd@DurationData{TDuration.grain = dg}:
       _) | g > dg -> Just . Token Duration $ duration g 1 <> dd
      _ -> Nothing
  }

ruleDurationYear :: Rule
ruleDurationYear = Rule
  { name = "<integer> year"
  , pattern =
    [ Predicate isNatural
    , regex "წელიწად(ის)?(ი)?(ში)?|წლი(ის)?(ში)?|წელშ?ი"
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = v}:_) ->
        Just . Token Duration . duration TG.Month $ 12 * floor v
      _ -> Nothing
  }

ruleDurationAndHalfYear :: Rule
ruleDurationAndHalfYear = Rule
  { name = "<integer> and an half year"
  , pattern =
    [ Predicate isNatural
    , regex "წელიწადნახევა?(რის)?(რი)?(რში)?|წლინახევრ(ის)?(არში)?"
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = v}:_) ->
        Just . Token Duration . duration TG.Month $ 6 + 12 * floor v
      _ -> Nothing
  }

ruleDurationAndHalfYear1 :: Rule
ruleDurationAndHalfYear1 = Rule
  { name = "<integer> and an half year"
  , pattern =
    [ regex "წელიწადნახევა?(რის)?(რი)?(რში)?|წლინახევრ(ის)?(არში)?"
    ]
  , prod = const $ Just . Token Duration . duration TG.Month $ 18
  }

ruleDurationAndHalfMonth :: Rule
ruleDurationAndHalfMonth = Rule
  { name = "<integer> and an half month"
  , pattern =
    [ Predicate isNatural
    , regex "თვენახევა?(რის)?(რი)?(რში)?"
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = v}:_) ->
        Just . Token Duration . duration TG.Day $ 15 + 30 * floor v
      _ -> Nothing
  }

ruleDurationAndHalfMonth1 :: Rule
ruleDurationAndHalfMonth1 = Rule
  { name = "month and an half month"
  , pattern =
    [ regex "თვენახევა?(რის)?(რი)?(რში)?"
    ]
  , prod = const $ Just . Token Duration . duration TG.Day $ 45
  }

ruleDurationAndHalfWeek :: Rule
ruleDurationAndHalfWeek = Rule
  { name = "<integer> and an half week"
  , pattern =
    [ Predicate isNatural
    , regex "თვენახევა?(რის)?(რი)?(რში)?"
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = v}:_) ->
        Just . Token Duration . duration TG.Day $ 3 + 7 * floor v
      _ -> Nothing
  }

ruleDurationAndHalfWeek1 :: Rule
ruleDurationAndHalfWeek1 = Rule
  { name = "week and an half week"
  , pattern =
    [ regex "კვირანახევა?(რის)?(რი)?(რში)?"
    ]
  , prod = const $ Just . Token Duration . duration TG.Day $ 10
  }

rules :: [Rule]
rules =
  [ ruleCompositeDuration
  , ruleCompositeDuration1
  , ruleDurationAndHalfYear
  , ruleDurationAndHalfYear1
  , ruleDurationAndHalfMonth
  , ruleDurationAndHalfMonth1
  , ruleDurationAndHalfWeek
  , ruleDurationAndHalfWeek1
  , ruleDurationYear
  ]
