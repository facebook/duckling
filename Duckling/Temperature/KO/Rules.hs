-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Temperature.KO.Rules
  ( rules ) where

import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Temperature.Helpers
import qualified Duckling.Temperature.Types as TTemperature
import Duckling.Types hiding (isLatent)

ruleLatentTempDegrees :: Rule
ruleLatentTempDegrees = Rule
  { name = "<latent temp> degrees"
  , pattern =
    [ Predicate $ isValueOnly False
    , regex "도|°"
    ]
  , prod = \tokens -> case tokens of
      (Token Temperature td:_) -> Just . Token Temperature $
        withUnit TTemperature.Degree td
      _ -> Nothing
  }

ruleTemp :: Rule
ruleTemp = Rule
  { name = "섭씨 <temp>"
  , pattern =
    [ regex "섭씨"
    , Predicate $ isValueOnly True
    ]
  , prod = \tokens -> case tokens of
      (_:Token Temperature td:_) -> Just . Token Temperature $
        withUnit TTemperature.Celsius td
      _ -> Nothing
  }

ruleTempC :: Rule
ruleTempC = Rule
  { name = "<temp> °C"
  , pattern =
    [ Predicate $ isValueOnly True
    , regex "c"
    ]
  , prod = \tokens -> case tokens of
      (Token Temperature td:_) -> Just . Token Temperature $
        withUnit TTemperature.Celsius td
      _ -> Nothing
  }

ruleTemp2 :: Rule
ruleTemp2 = Rule
  { name = "화씨 <temp>"
  , pattern =
    [ regex "화씨"
    , Predicate $ isValueOnly True
    ]
  , prod = \tokens -> case tokens of
      (_:Token Temperature td:_) -> Just . Token Temperature $
        withUnit TTemperature.Fahrenheit td
      _ -> Nothing
  }

ruleTempF :: Rule
ruleTempF = Rule
  { name = "<temp> °F"
  , pattern =
    [ Predicate $ isValueOnly True
    , regex "f"
    ]
  , prod = \tokens -> case tokens of
      (Token Temperature td:_) -> Just . Token Temperature $
        withUnit TTemperature.Fahrenheit td
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleLatentTempDegrees
  , ruleTemp
  , ruleTemp2
  , ruleTempC
  , ruleTempF
  ]
