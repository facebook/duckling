-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.KO.Rules
  ( rules ) where

import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Ordinal.Helpers
import Duckling.Numeral.Types (NumeralData (..))
import qualified Duckling.Numeral.Types as TNumeral
import Duckling.Types

ruleOrdinals :: Rule
ruleOrdinals = Rule
  { name = "ordinals (첫번째)"
  , pattern =
    [ dimension Numeral
    , regex "번째|째(번)?"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v}:_) ->
        Just . ordinal $ floor v
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleOrdinals
  ]
