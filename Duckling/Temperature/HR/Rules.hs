-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Temperature.HR.Rules
  ( rules ) where

import Data.String
import Prelude

import Duckling.Dimensions.Types
import Duckling.Temperature.Helpers
import Duckling.Temperature.Types (TemperatureData (..))
import Duckling.Types
import qualified Duckling.Temperature.Types as TTemperature

ruleLatentTempStupnjevi :: Rule
ruleLatentTempStupnjevi = Rule
  { name = "<latent temp> stupnjevi"
  , pattern =
    [ Predicate $ isValueOnly False
    , regex "deg\\.?|stupa?nj((ev)?a)?|°"
    ]
  , prod = \tokens -> case tokens of
      (Token Temperature td:_) -> Just . Token Temperature $
        withUnit TTemperature.Degree td
      _ -> Nothing
  }

ruleTempCelzij :: Rule
ruleTempCelzij = Rule
  { name = "<temp> Celzij"
  , pattern =
    [ Predicate $ isValueOnly True
    , regex "c(elz?(ija?)?)?\\.?"
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
    [ Predicate $ isValueOnly True
    , regex "f(ah?rh?eh?n(h?eit)?)?\\.?"
    ]
  , prod = \tokens -> case tokens of
      (Token Temperature td:_) -> Just . Token Temperature $
        withUnit TTemperature.Fahrenheit td
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleLatentTempStupnjevi
  , ruleTempCelzij
  , ruleTempFahrenheit
  ]
