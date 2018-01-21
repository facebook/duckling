-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Temperature.IT.Rules
  ( rules ) where

import Data.String
import Prelude

import Duckling.Dimensions.Types
import Duckling.Temperature.Helpers
import Duckling.Temperature.Types (TemperatureData (..))
import Duckling.Types
import qualified Duckling.Temperature.Types as TTemperature

ruleLatentTempDegrees :: Rule
ruleLatentTempDegrees = Rule
  { name = "<latent temp> degrees"
  , pattern =
    [ dimension Temperature
    , regex "(grad[io]?\\.?)|°"
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
    [ dimension Temperature
    , regex "(c((el[cs]?(ius)?)|(entigrad[io]))?\\.?)"
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
    , regex "f(ah?rh?eh?n(h?eit)?)?\\.?"
    ]
  , prod = \tokens -> case tokens of
      (Token Temperature td:_) -> Just . Token Temperature $
        withUnit TTemperature.Fahrenheit td
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleLatentTempDegrees
  , ruleTempCelcius
  , ruleTempFahrenheit
  ]
