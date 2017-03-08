-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Temperature.GA.Rules
  ( rules ) where

import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Temperature.Helpers
import Duckling.Temperature.Types (TemperatureData (..))
import qualified Duckling.Temperature.Types as TTemperature
import Duckling.Types

ruleLatentTempCim :: Rule
ruleLatentTempCim = Rule
  { name = "<latent temp> céim"
  , pattern =
    [ dimension Temperature
    , regex "g?ch?(\x00e9|e)im(e(anna)?)?|\x00b0"
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
    , regex "ceinteagr(\x00e1|a)d|c(el[cs]?(ius)?)?\\.?"
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

ruleLatentTempFaoiBhunNid :: Rule
ruleLatentTempFaoiBhunNid = Rule
  { name = "<latent temp> faoi bhun náid"
  , pattern =
    [ dimension Temperature
    , regex "faoi bhun (0|n(a|\x00e1)id)"
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
  [ ruleLatentTempCim
  , ruleLatentTempFaoiBhunNid
  , ruleTempCelsius
  , ruleTempFahrenheit
  ]
