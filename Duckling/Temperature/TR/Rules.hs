-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Temperature.TR.Rules
  ( rules ) where

import Data.Maybe
import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Temperature.Helpers
import qualified Duckling.Temperature.Types as TTemperature
import Duckling.Temperature.Types (TemperatureData(..))
import Duckling.Types

ruleTemperatureDegrees :: Rule
ruleTemperatureDegrees = Rule
  { name = "<latent temp> degrees"
  , pattern =
    [ Predicate $ isValueOnly False
    , regex "derece|°"
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
    , regex "c|santigrat"
    ]
  , prod = \case
    (Token Temperature td:_) -> Just . Token Temperature $
      withUnit TTemperature.Celsius td
    _ -> Nothing
  }

ruleTemperatureFahrenheit :: Rule
ruleTemperatureFahrenheit = Rule
  { name = "<temp> Fahrenheit"
  , pattern =
    [ Predicate $ isValueOnly True
    , regex "f(ahrenh?ayt)?"
    ]
  , prod = \case
    (Token Temperature td:_) -> Just . Token Temperature $
      withUnit TTemperature.Fahrenheit td
    _ -> Nothing
  }

ruleTemperatureBelowZero :: Rule
ruleTemperatureBelowZero = Rule
  { name = "below zero <temp>"
  , pattern =
    [ regex "s\305f\305r\305n alt\305nda"
    , Predicate $ isValueOnly True
    ]
  , prod = \case
      (_:Token Temperature td@TemperatureData{TTemperature.value = Just v}:_) ->
        case TTemperature.unit td of
          Nothing -> Just . Token Temperature . withUnit TTemperature.Degree $
            td {TTemperature.value = Just (- v)}
          _ -> Just . Token Temperature $ td {TTemperature.value = Just (- v)}
      _ -> Nothing
  }

ruleTemperatureBelowZeroReverse :: Rule
ruleTemperatureBelowZeroReverse = Rule
  { name = "<temp> below zero"
  , pattern =
    [ Predicate $ isValueOnly True
    , regex "s\305f\305r\305n alt\305nda"
    ]
  , prod = \case
      (Token Temperature td@TemperatureData{TTemperature.value = Just v}:_) ->
        case TTemperature.unit td of
          Nothing -> Just . Token Temperature . withUnit TTemperature.Degree $
            td {TTemperature.value = Just (- v)}
          _ -> Just . Token Temperature $ td {TTemperature.value = Just (- v)}
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleTemperatureDegrees
  , ruleTemperatureCelsius
  , ruleTemperatureFahrenheit
  , ruleTemperatureBelowZero
  , ruleTemperatureBelowZeroReverse
  ]
