-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Quantity.RO.Rules
  ( rules ) where

import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Number.Types (NumberData (..))
import qualified Duckling.Number.Types as TNumber
import Duckling.Quantity.Helpers
import qualified Duckling.Quantity.Types as TQuantity
import Duckling.Regex.Types
import Duckling.Types

ruleNumberUnits :: Rule
ruleNumberUnits = Rule
  { name = "<number> <units>"
  , pattern =
    [ dimension DNumber
    , regex "livr(a|e|\x0103)"
    ]
  , prod = \tokens -> case tokens of
      (Token DNumber NumberData {TNumber.value = v}:_) ->
        Just . Token Quantity $ quantity TQuantity.Pound v
      _ -> Nothing
  }

ruleQuantityOfProduct :: Rule
ruleQuantityOfProduct = Rule
  { name = "<quantity> of product"
  , pattern =
    [ dimension Quantity
    , regex "de (carne|can(a|\x0103)|zah(a|\x0103)r)"
    ]
  , prod = \tokens -> case tokens of
      (Token Quantity qd:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> Just . Token Quantity $ withProduct match qd
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleNumberUnits
  , ruleQuantityOfProduct
  ]
