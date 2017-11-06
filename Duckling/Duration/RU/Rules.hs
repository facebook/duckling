-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.RU.Rules
  ( rules
  ) where

import Data.String
import Prelude
import qualified Data.Text as Text
import Data.Text (Text)

import Duckling.Dimensions.Types
import Duckling.Duration.Helpers
import Duckling.Numeral.Helpers (numberWith)
import Duckling.Numeral.Types (NumeralData(..), isInteger)
import Duckling.Duration.Types (DurationData (DurationData))
import Duckling.Regex.Types
import Duckling.Types
import qualified Duckling.Numeral.Types as TNumeral
import Duckling.TimeGrain.Types

rulesHalfAsSingleWord :: [Rule]
rulesHalfAsSingleWord = map toRule halvesAsAWord
  where
    halvesAsAWord :: [(Text, String, Grain, Int)]
    halvesAsAWord =
      [ ("half of a year", "пол\\s?года", Month, 6)
      , ("half of a month", "пол\\s?месяца", Day, 15)
      , ("half of a day", "пол\\s?дня", Hour, 12)
      , ("half of a hour", "пол\\s?часа", Minute, 30)
      , ("half of a minute", "пол\\s?минуты", Second, 30)
      ]

    toRule :: (Text, String, Grain, Int) -> Rule
    toRule (name, regexPattern, grain, quantity) = Rule
      { name = name
      , pattern = [ regex regexPattern ]
      , prod = \tokens -> case tokens of
          (Token RegexMatch _:_) ->
            Just . Token Duration $ duration grain quantity
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
      (Token Numeral (NumeralData {TNumeral.value = v}):
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
    [ regex "(где-то|приблизительно|примерно|ровно)"
    , dimension Duration
    ]
    , prod = \tokens -> case tokens of
        (_:token:_) -> Just token
        _ -> Nothing
  }

ruleGrainAsDuration :: Rule
ruleGrainAsDuration = Rule
  { name = "a <unit-of-duration>"
  , pattern = [ dimension TimeGrain ]
  , prod = \tokens -> case tokens of
      (Token TimeGrain grain:_) ->
        Just . Token Duration $ duration grain 1
      _ -> Nothing
  }

rulePositiveDuration :: Rule
rulePositiveDuration = Rule
  { name = "<positive-numeral> <time-grain>"
  , pattern =
    [ numberWith TNumeral.value (> 0)
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = v}):Token TimeGrain grain:_) ->
        Just . Token Duration $ toDurationData grain v
      _ -> Nothing
  }
  where
    toDurationData :: Grain -> Double -> DurationData
    toDurationData grain value
      | isInteger value = duration grain $ floor value
      | otherwise = duration Second . floor $ inSeconds grain value

rules :: [Rule]
rules =
  [ rulePositiveDuration
  , ruleDurationPrecision
  , ruleNumeralQuotes
  , ruleGrainAsDuration
  ] ++ rulesHalfAsSingleWord
