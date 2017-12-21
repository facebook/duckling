-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Temperature.AR.Rules
  ( rules
  ) where

import Data.Maybe
import Data.String
import Prelude

import Duckling.Dimensions.Types
import Duckling.Temperature.Helpers
import Duckling.Temperature.Types (TemperatureData(..))
import Duckling.Types
import qualified Duckling.Temperature.Types as TTemperature

ruleTemperatureDegrees :: Rule
ruleTemperatureDegrees = Rule
  { name = "<latent temp> degrees"
  , pattern =
    [ dimension Temperature
    , regex "(درج([ةه]|ات)( مئوي[ةه])?)|°"
    ]
  , prod = \tokens -> case tokens of
      (Token Temperature td:_) -> Just . Token Temperature $
        withUnit TTemperature.Degree td
      _ -> Nothing
  }

ruleTemperatureTwoDegrees :: Rule
ruleTemperatureTwoDegrees = Rule
  { name = "two degrees"
  , pattern =
    [ regex "درجت(ين|ان)"
    ]
  , prod = \_ -> Just . Token Temperature $ TemperatureData
      { TTemperature.unit = Nothing
      , TTemperature.value = 2
      }
  }

ruleTemperatureCelsius :: Rule
ruleTemperatureCelsius = Rule
  { name = "<temp> Celsius"
  , pattern =
    [ dimension Temperature
    , regex "(درج([ةه]|ات) )?سي?لي?[سز]ي?وس"
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
    [ dimension Temperature
    , regex "(درج([ةه]|ات) )?ف(ا|ي)?هرنها?يت"
    ]
  , prod = \tokens -> case tokens of
    (Token Temperature td:_) -> Just . Token Temperature $
      withUnit TTemperature.Fahrenheit td
    _ -> Nothing
  }

ruleTemperatureBelowZero :: Rule
ruleTemperatureBelowZero = Rule
  { name = "<temp> below zero"
  , pattern =
    [ dimension Temperature
    , regex "تحت الصفر"
    ]
  , prod = \tokens -> case tokens of
      (Token Temperature td@TemperatureData {TTemperature.value = v}:
       _) -> case TTemperature.unit td of
        Nothing -> Just . Token Temperature . withUnit TTemperature.Degree $
          td {TTemperature.value = - v}
        _ -> Just . Token Temperature $ td {TTemperature.value = - v}
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleTemperatureDegrees
  , ruleTemperatureTwoDegrees
  , ruleTemperatureCelsius
  , ruleTemperatureFahrenheit
  , ruleTemperatureBelowZero
  ]
