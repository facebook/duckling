-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Temperature.CS.Rules
  ( rules ) where

import Data.Maybe
import Data.String
import Prelude

import Duckling.Dimensions.Types
import Duckling.Temperature.Helpers
import Duckling.Temperature.Types (TemperatureData(..), unitsAreCompatible)
import Duckling.Types
import qualified Duckling.Temperature.Types as TTemperature

ruleTemperatureDegrees :: Rule
ruleTemperatureDegrees = Rule
  { name = "<latent temp> degrees"
  , pattern =
    [ Predicate $ isValueOnly False
    , regex "(stup(e[n\x0148]|n[e\x011b]|[n\x0148][u\x016f]))|°"
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
    , regex "c(elsia)?"
    ]
  , prod = \case
      (Token Temperature td:_) -> Just . Token Temperature $
        withUnit TTemperature.Celsius td
      _ -> Nothing
  }

ruleTemperatureBelowZero :: Rule
ruleTemperatureBelowZero = Rule
  { name = "<temp> below zero"
  , pattern =
    [ Predicate $ isValueOnly True
    , regex "pod nulou"
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
  , ruleTemperatureCelsius
  , ruleTemperatureBelowZero
  ]