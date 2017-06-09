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

import Data.String
import Prelude
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Quantity.Helpers
import Duckling.Regex.Types
import Duckling.Types
import qualified Duckling.Numeral.Types as TNumeral
import qualified Duckling.Quantity.Types as TQuantity

ruleNumeralQuantity :: Rule
ruleNumeralQuantity = Rule
  { name = "<number> <quantity>"
  , pattern =
    [ dimension Numeral
    , regex "((pound|cup|gram)s?|g)"
    ]
  , prod = \tokens -> case tokens of
    (Token Numeral nd:
     Token RegexMatch (GroupMatch (match:_)):
     _) -> case Text.toLower match of
      "cup"    ->
        Just . Token Quantity . quantity TQuantity.Cup $ TNumeral.value nd
      "cups"   ->
        Just . Token Quantity . quantity TQuantity.Cup $ TNumeral.value nd
      "g"      ->
        Just . Token Quantity . quantity TQuantity.Gram $ TNumeral.value nd
      "gram"   ->
        Just . Token Quantity . quantity TQuantity.Gram $ TNumeral.value nd
      "grams"  ->
        Just . Token Quantity . quantity TQuantity.Gram $ TNumeral.value nd
      "pound"  ->
        Just . Token Quantity . quantity TQuantity.Pound $ TNumeral.value nd
      "pounds" ->
        Just . Token Quantity . quantity TQuantity.Pound $ TNumeral.value nd
      _        -> Nothing
    _ -> Nothing
  }

ruleAQuantity :: Rule
ruleAQuantity = Rule
  { name = "a quantity"
  , pattern =
    [ regex "a (pound|cup|gram)s?"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "cup"    -> Just . Token Quantity $ quantity TQuantity.Cup 1
        "gram"   -> Just . Token Quantity $ quantity TQuantity.Gram 1
        "pound"  -> Just . Token Quantity $ quantity TQuantity.Pound 1
        _        -> Nothing
      _ -> Nothing
  }

ruleQuantityOfProduct :: Rule
ruleQuantityOfProduct = Rule
  { name = "<quantity> of product"
  , pattern =
    [ dimension Quantity
    , regex "of (\\w+)"
    ]
  , prod = \tokens -> case tokens of
    (Token Quantity qd:Token RegexMatch (GroupMatch (product:_)):_) ->
      Just . Token Quantity $ withProduct product qd
    _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleNumeralQuantity
  , ruleAQuantity
  , ruleQuantityOfProduct
  ]
