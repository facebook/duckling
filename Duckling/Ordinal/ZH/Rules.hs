-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.ZH.Rules
  ( rules ) where

import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Number.Types (NumberData(..))
import qualified Duckling.Number.Types as TNumber
import Duckling.Ordinal.Helpers
import Duckling.Types

ruleOrdinalDigits :: Rule
ruleOrdinalDigits = Rule
  { name = "ordinal (digits)"
  , pattern =
    [ regex "\x7b2c"
    , dimension DNumber
    ]
  , prod = \tokens -> case tokens of
      (_:Token DNumber (NumberData {TNumber.value = x}):_) ->
        Just . ordinal $ floor x
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleOrdinalDigits
  ]
