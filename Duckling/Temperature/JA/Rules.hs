-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Temperature.JA.Rules
  ( rules ) where

import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Temperature.Helpers
import qualified Duckling.Temperature.Types as TTemperature
import Duckling.Types

ruleLatentTempDegrees :: Rule
ruleLatentTempDegrees = Rule
  { name = "<latent temp> degrees"
  , pattern =
    [ Predicate $ isValueOnly False
    , regex "度|°"
    ]
  , prod = \tokens -> case tokens of
      (Token Temperature td:_) -> Just . Token Temperature $
        withUnit TTemperature.Degree td
      _ -> Nothing
  }

ruleTempCelcius :: Rule
ruleTempCelcius = Rule
  { name = "<temp> Celcius"
  , pattern =
    [ Predicate $ isValueOnly True
    , regex "摂氏(°|度)|(°)C"
    ]
  , prod = \tokens -> case tokens of
      (Token Temperature td:_) -> Just . Token Temperature $
        withUnit TTemperature.Celsius td
      _ -> Nothing
  }

ruleCelciusTemp :: Rule
ruleCelciusTemp = Rule
  { name = "Celcius <temp>"
  , pattern =
    [ regex "摂氏"
    , Predicate $ isValueOnly True
    , regex "度|°"
    ]
  , prod = \tokens -> case tokens of
      (_:Token Temperature td:_) -> Just . Token Temperature $
        withUnit TTemperature.Celsius td
      _ -> Nothing
  }

ruleTempFahrenheit :: Rule
ruleTempFahrenheit = Rule
  { name = "<temp> Fahrenheit"
  , pattern =
    [ Predicate $ isValueOnly True
    , regex "華氏(°|度)|(°)F"
    ]
  , prod = \tokens -> case tokens of
      (Token Temperature td:_) -> Just . Token Temperature $
        withUnit TTemperature.Fahrenheit td
      _ -> Nothing
  }

ruleFahrenheitTemp :: Rule
ruleFahrenheitTemp = Rule
  { name = "Fahrenheit <temp>"
  , pattern =
    [ regex "華氏"
    , Predicate $ isValueOnly True
    , regex "度|°"
    ]
  , prod = \tokens -> case tokens of
      (_:Token Temperature td:_) -> Just . Token Temperature $
        withUnit TTemperature.Fahrenheit td
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleCelciusTemp
  , ruleFahrenheitTemp
  , ruleLatentTempDegrees
  , ruleTempCelcius
  , ruleTempFahrenheit
  ]
