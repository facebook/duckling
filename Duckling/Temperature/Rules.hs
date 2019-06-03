-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Temperature.Rules
  ( rules
  ) where

import Data.String
import Prelude

import Duckling.Dimensions.Types
import Duckling.Temperature.Helpers
import Duckling.Temperature.Types (TemperatureData (..))
import Duckling.Types
import qualified Duckling.Numeral.Types as TNumeral
import qualified Duckling.Temperature.Types as TTemperature

ruleNumeralAsTemp :: Rule
ruleNumeralAsTemp = Rule
  { name = "number as temp"
  , pattern =
    [ dimension Numeral
    ]
  , prod = \case
      (Token Numeral nd:_) ->
        Just . Token Temperature $ valueOnly $ floor $ TNumeral.value nd
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleNumeralAsTemp
  ]
