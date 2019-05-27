-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Temperature.RO.Rules
  ( rules
  ) where

import Data.String
import Prelude

import Duckling.Dimensions.Types
import Duckling.Temperature.Helpers
import Duckling.Types
import qualified Duckling.Temperature.Types as TTemperature

ruleLatentTempGrade :: Rule
ruleLatentTempGrade = Rule
  { name = "<latent temp> grade"
  , pattern =
    [ Predicate $ isValueOnly False
    , regex "(de )?(grade)|°"
    ]
  , prod = \case
      (Token Temperature td:_) -> Just . Token Temperature $
        withUnit TTemperature.Degree td
      _ -> Nothing
  }

ruleTempCelcius :: Rule
ruleTempCelcius = Rule
  { name = "<temp> Celcius"
  , pattern =
    [ Predicate $ isValueOnly True
    , regex "(de )?c(el[cs]?(ius)?)?\\.?"
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
    , regex "(de )?f(ah?rh?eh?n(h?eit)?)?\\.?"
    ]
  , prod = \case
      (Token Temperature td:_) -> Just . Token Temperature $
        withUnit TTemperature.Fahrenheit td
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleLatentTempGrade
  , ruleTempCelcius
  , ruleTempFahrenheit
  ]
