-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Distance.Rules
  ( rules
  ) where


import Data.String
import Prelude

import Duckling.Dimensions.Types
import Duckling.Distance.Helpers
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Types
import qualified Duckling.Numeral.Types as TNumeral

ruleNumeralAsDistance :: Rule
ruleNumeralAsDistance = Rule
  { name = "number as distance"
  , pattern =
    [ dimension Numeral
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData {TNumeral.value = v}:_) ->
        Just . Token Distance $ distance v
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleNumeralAsDistance
  ]
