-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.KO.Rules
  ( rules ) where

import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Ordinal.Helpers
import Duckling.Number.Types (NumberData (..))
import qualified Duckling.Number.Types as TNumber
import Duckling.Types

ruleOrdinals :: Rule
ruleOrdinals = Rule
  { name = "ordinals (첫번째)"
  , pattern =
    [ dimension Numeral
    , regex "\xbc88\xc9f8|\xc9f8(\xbc88)?"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumberData {TNumber.value = v}):_) ->
        Just . ordinal $ floor v
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleOrdinals
  ]
