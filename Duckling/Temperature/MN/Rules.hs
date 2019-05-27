-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Temperature.MN.Rules
  ( rules ) where

import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Temperature.Helpers
import Duckling.Temperature.Types (TemperatureData(..))
import qualified Duckling.Temperature.Types as TTemperature
import Duckling.Types

ruleTemperatureDegrees :: Rule
ruleTemperatureDegrees = Rule
  { name = "<latent temp> градус"
  , pattern =
    [ Predicate $ isValueOnly False
    , regex "градус|°|хэм"
    ]
  , prod = \case
      (Token Temperature td:_) -> Just . Token Temperature $
        withUnit TTemperature.Degree td
      _ -> Nothing
  }

ruleTemperatureCelsius :: Rule
ruleTemperatureCelsius = Rule
  { name = "<temp> Celsius"
  , pattern =
    [ Predicate $ isValueOnly True
    , regex "c(el[cs]?(ius)?)?\\.?"
    ]
  , prod = \case
      (Token Temperature td:_) -> Just . Token Temperature $
        withUnit TTemperature.Celsius td
      _ -> Nothing
  }

ruleTempC :: Rule
ruleTempC = Rule
  { name = "<temp> °C"
  , pattern =
    [ Predicate $ isValueOnly True
    , regex "c"
    ]
  , prod = \tokens -> case tokens of
      (Token Temperature td:_) -> Just . Token Temperature $
        withUnit TTemperature.Celsius td
      _ -> Nothing
  }

ruleTemperatureFahrenheit :: Rule
ruleTemperatureFahrenheit = Rule
  { name = "<temp> Fahrenheit"
  , pattern =
    [ Predicate $ isValueOnly True
    , regex "((f(ah?rh?eh?n(h?eit)?)?\\.?)|фарангейт)"
    ]
  , prod = \case
      (Token Temperature td:_) -> Just . Token Temperature $
        withUnit TTemperature.Fahrenheit td
      _ -> Nothing
  }

ruleTemperatureBelowZero :: Rule
ruleTemperatureBelowZero = Rule
  { name = "<temp> below zero"
  , pattern =
    [ Predicate $ isValueOnly True
    , regex "тэгээс доош"
    ]
  , prod = \case
      (Token Temperature td@TemperatureData {TTemperature.value = Just v}:
       _) -> case TTemperature.unit td of
        Nothing -> Just . Token Temperature . withUnit TTemperature.Degree $
          td {TTemperature.value = Just (- v)}
        _ -> Just . Token Temperature $ td {TTemperature.value = Just (- v)}
      _ -> Nothing
  }



ruleIntervalMax :: Rule
ruleIntervalMax = Rule
  { name = "under/less/lower/no more than <temp>"
  , pattern =
    [ regex "доогуур|(бага|ихгүй|их биш)"
    , Predicate isSimpleTemperature
    ]
  , prod = \case
      (_:
       Token Temperature TemperatureData{TTemperature.value = Just to,
                                         TTemperature.unit = Just u}:
       _) -> Just . Token Temperature . withMax to $ unitOnly u
      _ -> Nothing
  }

ruleIntervalMin :: Rule
ruleIntervalMin = Rule
  { name = "over/above/at least/more than <temp>"
  , pattern =
    [ regex "дээгүүр|их|багадаа"
    , Predicate isSimpleTemperature
    ]
  , prod = \case
      (_:
       Token Temperature TemperatureData{TTemperature.value = Just from,
                                         TTemperature.unit = Just u}:
       _) -> Just . Token Temperature . withMin from $ unitOnly u
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleTemperatureDegrees
  , ruleTemperatureCelsius
  , ruleTemperatureFahrenheit
  , ruleTemperatureBelowZero
  , ruleTempC
  , ruleIntervalMin
  , ruleIntervalMax
  ]
