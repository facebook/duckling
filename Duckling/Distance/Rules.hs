-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Distance.Rules
  ( rules
  ) where


import Data.String
import Prelude

import Duckling.Dimensions.Types
import Duckling.Distance.Helpers
import Duckling.Number.Types (NumberData (..))
import qualified Duckling.Number.Types as TNumber
import Duckling.Types

ruleNumberAsDistance :: Rule
ruleNumberAsDistance = Rule
  { name = "number as distance"
  , pattern =
    [ dimension DNumber
    ]
  , prod = \tokens -> case tokens of
      (Token DNumber NumberData {TNumber.value = v}:_) ->
        Just . Token Distance $ distance v
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleNumberAsDistance
  ]
