-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Quantity.FR.Rules
  ( rules ) where

import qualified Data.Text as Text
import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Quantity.Helpers
import qualified Duckling.Quantity.Types as TQuantity
import Duckling.Number.Types (NumberData (..))
import qualified Duckling.Number.Types as TNumber
import Duckling.Regex.Types
import Duckling.Types

ruleNumberUnits :: Rule
ruleNumberUnits = Rule
  { name = "<number> <units>"
  , pattern =
    [ dimension Numeral
    , regex "(tasses?|cuill?(e|\x00e8)res? (a|\x00e0) soupe?)"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumberData {TNumber.value = v}:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> case Text.toLower match of
         "tasse"  -> Just . Token Quantity $ quantity TQuantity.Cup v
         "tasses" -> Just . Token Quantity $ quantity TQuantity.Cup v
         _        -> Just . Token Quantity $ quantity TQuantity.Tablespoon v
      _ -> Nothing
  }

ruleQuantityOfProduct :: Rule
ruleQuantityOfProduct = Rule
  { name = "<quantity> of product"
  , pattern =
    [ dimension Quantity
    , regex "de (caf(e|\x00e9)|sucre)"
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
