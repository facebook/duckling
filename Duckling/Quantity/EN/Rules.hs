-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Quantity.EN.Rules
  ( rules ) where

import qualified Data.Text as Text
import Prelude
import Data.String

import qualified Duckling.Number.Types as TNumber
import Duckling.Quantity.Helpers
import qualified Duckling.Quantity.Types as TQuantity
import Duckling.Regex.Types
import Duckling.Dimensions.Types
import Duckling.Types

ruleNumberQuantity :: Rule
ruleNumberQuantity = Rule
  { name = "<number> <quantity>"
  , pattern =
    [ dimension Numeral
    , regex "(pound|cup)s?"
    ]
  , prod = \tokens -> case tokens of
    (Token Numeral nd:
     Token RegexMatch (GroupMatch (match:_)):
     _) -> case Text.toLower match of
      "cup"    -> Just . Token Quantity . quantity TQuantity.Cup $ TNumber.value nd
      "cups"   -> Just . Token Quantity . quantity TQuantity.Cup $ TNumber.value nd
      "pound"  -> Just . Token Quantity . quantity TQuantity.Pound $ TNumber.value nd
      "pounds" -> Just . Token Quantity . quantity TQuantity.Pound $ TNumber.value nd
      _ -> Nothing
    _ -> Nothing
  }

ruleAQuantity :: Rule
ruleAQuantity = Rule
  { name = "a quantity"
  , pattern = [ regex "a (pound|cup)s?"]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "cup"    -> Just . Token Quantity $ quantity TQuantity.Cup 1
        "cups"   -> Just . Token Quantity $ quantity TQuantity.Cup 1
        "pound"  -> Just . Token Quantity $ quantity TQuantity.Pound 1
        "pounds" -> Just . Token Quantity $ quantity TQuantity.Pound 1
        _        -> Nothing
      _ -> Nothing
  }

ruleQuantityOfProduct :: Rule
ruleQuantityOfProduct = Rule
  { name = "<quantity> of product"
  , pattern =
    [ dimension Quantity
    , regex "of (meat|sugar)"
    ]
  , prod = \tokens -> case tokens of
    (Token Quantity qd:Token RegexMatch (GroupMatch (product:_)):_) ->
      Just . Token Quantity $ withProduct product qd
    _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleNumberQuantity
  , ruleAQuantity
  , ruleQuantityOfProduct
  ]
