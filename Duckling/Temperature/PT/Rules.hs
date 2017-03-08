-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Temperature.PT.Rules
  ( rules ) where

import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Temperature.Helpers
import Duckling.Temperature.Types (TemperatureData (..))
import qualified Duckling.Temperature.Types as TTemperature
import Duckling.Types

ruleLatentTempTemp :: Rule
ruleLatentTempTemp = Rule
  { name = "<latent temp> temp"
  , pattern =
    [ dimension Temperature
    , regex "(graus?)|\x00b0"
    ]
  , prod = \tokens -> case tokens of
      (Token Temperature td:_) -> Just . Token Temperature $
        withUnit TTemperature.Degree td
      _ -> Nothing
  }

ruleTempCelsius :: Rule
ruleTempCelsius = Rule
  { name = "<temp> Celsius"
  , pattern =
    [ dimension Temperature
    , regex "(cent(i|\x00ed)grados?|c(el[cs]?(ius)?)?\\.?)"
    ]
  , prod = \tokens -> case tokens of
      (Token Temperature td:_) -> Just . Token Temperature $
        withUnit TTemperature.Celsius td
      _ -> Nothing
  }

ruleTempFahrenheit :: Rule
ruleTempFahrenheit = Rule
  { name = "<temp> Fahrenheit"
  , pattern =
    [ dimension Temperature
    , regex "f(ah?reh?n(h?eit)?)?\\.?"
    ]
  , prod = \tokens -> case tokens of
      (Token Temperature td:_) -> Just . Token Temperature $
        withUnit TTemperature.Fahrenheit td
      _ -> Nothing
  }

ruleLatentTempTempAbaixoDeZero :: Rule
ruleLatentTempTempAbaixoDeZero = Rule
  { name = "<latent temp> temp abaixo de zero"
  , pattern =
    [ dimension Temperature
    , regex "((graus?)|\x00b0)?( abaixo (de)? zero)"
    ]
  , prod = \tokens -> case tokens of
      (Token Temperature td@(TemperatureData {TTemperature.value = v}):_) ->
        case TTemperature.unit td of
          Nothing -> Just . Token Temperature . withUnit TTemperature.Degree $
            td {TTemperature.value = - v}
          _ -> Just . Token Temperature $ td {TTemperature.value = - v}
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleLatentTempTemp
  , ruleLatentTempTempAbaixoDeZero
  , ruleTempCelsius
  , ruleTempFahrenheit
  ]
