-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Temperature.ES.Rules
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
    [ Predicate $ isValueOnly False
    , regex "(grados?)|°"
    ]
  , prod = \case
      (Token Temperature td:_) -> Just . Token Temperature $
        withUnit TTemperature.Degree td
      _ -> Nothing
  }

ruleTempCelsius :: Rule
ruleTempCelsius = Rule
  { name = "<temp> Celsius"
  , pattern =
    [ Predicate $ isValueOnly True
    , regex "(cent(i|í)grados?|c(el[cs]?(ius)?)?\\.?)"
    ]
  , prod = \case
      (Token Temperature td:_) -> Just . Token Temperature $
        withUnit TTemperature.Celsius td
      _ -> Nothing
  }

ruleTempFahrenheit :: Rule
ruleTempFahrenheit = Rule
  { name = "<temp> Fahrenheit"
  , pattern =
    [ Predicate $ isValueOnly True
    , regex "f(ah?reh?n(h?eit)?)?\\.?"
    ]
  , prod = \case
      (Token Temperature td:_) -> Just . Token Temperature $
        withUnit TTemperature.Fahrenheit td
      _ -> Nothing
  }

ruleLatentTempTempBajoCero :: Rule
ruleLatentTempTempBajoCero = Rule
  { name = "<latent temp> temp bajo cero"
  , pattern =
    [ Predicate $ isValueOnly True
    , regex "bajo cero"
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
  [ ruleLatentTempTemp
  , ruleLatentTempTempBajoCero
  , ruleTempCelsius
  , ruleTempFahrenheit
  ]
