-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
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
    [ Predicate $ isValueOnly False
    , regex "(درج([ةه]|ات)( مئوي[ةه])?)|°"
    ]
  , prod = \case
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
  , prod = \_ -> Just . Token Temperature $ valueOnly 2
  }

ruleTemperatureCelsius :: Rule
ruleTemperatureCelsius = Rule
  { name = "<temp> Celsius"
  , pattern =
    [ Predicate $ isValueOnly True
    , regex "(درج([ةه]|ات) )?سي?لي?[سز]ي?وس"
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
    , regex "(درج([ةه]|ات) )?ف(ا|ي)?هرنها?يت"
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
    , regex "تحت الصفر"
    ]
  , prod = \case
      (Token Temperature td@TemperatureData {TTemperature.value = Just v}:
       _) -> case TTemperature.unit td of
        Nothing -> Just . Token Temperature . withUnit TTemperature.Degree $
          td {TTemperature.value = Just (- v)}
        _ -> Just . Token Temperature $ td {TTemperature.value = Just (- v)}
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
