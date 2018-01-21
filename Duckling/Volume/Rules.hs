-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Volume.Rules
  ( rules
  ) where

import Data.String
import Prelude

import Duckling.Dimensions.Types
import Duckling.Numeral.Types (NumeralData(..))
import Duckling.Types
import Duckling.Volume.Helpers
import qualified Duckling.Numeral.Types as TNumeral

ruleNumeralAsVolume :: Rule
ruleNumeralAsVolume = Rule
  { name = "number as volume"
  , pattern =
    [ dimension Numeral
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData {TNumeral.value = v}:_) ->
        Just . Token Volume $ volume v
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleNumeralAsVolume
  ]
