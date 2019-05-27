-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Temperature.KM.Rules
  ( rules
  ) where

import Data.String
import Prelude

import Duckling.Dimensions.Types
import Duckling.Temperature.Helpers
import Duckling.Types
import qualified Duckling.Temperature.Types as TTemperature

ruleLatentTempDegrees :: Rule
ruleLatentTempDegrees = Rule
  { name = "<latent temp> degrees"
  , pattern =
    [ Predicate $ isValueOnly False
    , regex "ដឺក្រេ|°"
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
    , regex "អង្សា(សេ)?"
    ]
  , prod = \case
      (Token Temperature td:_) -> Just . Token Temperature $
        withUnit TTemperature.Celsius td
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleTempCelcius
  , ruleLatentTempDegrees
  ]
