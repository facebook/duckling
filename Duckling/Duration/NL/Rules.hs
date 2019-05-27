-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.NL.Rules
  ( rules ) where

import Control.Monad (join)
import Prelude
import Data.String
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
  , pattern = [ regex "1/4\\s?(h|u(ur)?)|kwartier"]
  , prod = \_ -> Just . Token Duration $ duration TG.Minute 15
  }

ruleDurationKwartier :: Rule
ruleDurationKwartier = Rule
  { name = "<integer> kwartier"
  , pattern =
    [ Predicate isNatural
    , regex "kwartier"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v}:_) ->
        Just . Token Duration . duration TG.Minute $ 15 * floor v
      _ -> Nothing
  }

ruleDurationHalfAnHour :: Rule
ruleDurationHalfAnHour = Rule
  { name = "half an hour"
  , pattern = [regex "(1/2\\s?uur|half uur)"]
  , prod = \_ -> Just . Token Duration $ duration TG.Minute 30
  }

ruleDurationThreeQuartersOfAnHour :: Rule
ruleDurationThreeQuartersOfAnHour = Rule
  { name = "three-quarters of an hour"
  , pattern = [regex "3/4\\s?uur"]
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
  { name = "number,number uur"
  , pattern = [regex "(\\d+)\\,(\\d+) *(uur|uren)"]
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
    , regex "en een half (uur|uren)"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v}:_) ->
        Just . Token Duration . duration TG.Minute $ 30 + 60 * floor v
      _ -> Nothing
  }

ruleDurationPrecision :: Rule
ruleDurationPrecision = Rule
  { name = "about|exactly <duration>"
  , pattern =
    [ regex "(ongeveer|precies|plusminus|exact)"
    , dimension Duration
    ]
    , prod = \tokens -> case tokens of
        (_:token:_) -> Just token
        _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleDurationQuarterOfAnHour
  , ruleDurationKwartier
  , ruleDurationHalfAnHour
  , ruleDurationThreeQuartersOfAnHour
  , ruleDurationDotNumeralHours
  , ruleDurationAndHalfHour
  , ruleDurationPrecision
  , ruleNumeralQuotes
  ]
