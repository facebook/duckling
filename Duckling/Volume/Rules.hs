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
import Duckling.Number.Types (NumberData(..))
import qualified Duckling.Number.Types as TNumber
import Duckling.Types
import Duckling.Volume.Helpers

ruleNumberAsVolume :: Rule
ruleNumberAsVolume = Rule
  { name = "number as volume"
  , pattern =
    [ dimension DNumber
    ]
  , prod = \tokens -> case tokens of
      (Token DNumber NumberData {TNumber.value = v}:_) ->
        Just . Token Volume $ volume v
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleNumberAsVolume
  ]
