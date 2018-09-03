-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Quantity.KM.Rules
  ( rules ) where

import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Numeral.Types (NumeralData (..))
import qualified Duckling.Numeral.Types as TNumeral
import Duckling.Quantity.Helpers
import qualified Duckling.Quantity.Types as TQuantity
import Duckling.Regex.Types
import Duckling.Types

ruleQuantityOfProduct :: Rule
ruleQuantityOfProduct = Rule
  { name = "<quantity> of product"
  , pattern =
    [ regex "(មនុស្ស|បងប្អូន|សត្វ|ឆ្កែ|ឆ្មា|ដើមឈើ|ផ្កា|កុលាប|ផ្ទះ)"
    , dimension Quantity
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):
       Token Quantity qd:
       _) -> Just . Token Quantity $ withProduct match qd
      _ -> Nothing
  }

ruleNumeralUnits :: Rule
ruleNumeralUnits = Rule
  { name = "<number><units>"
  , pattern =
    [ dimension Numeral
    , regex "(នាក់|ក្បាល|ដើម|ទង|ខ្នង|មុខ|ចាន|ពែង|កែវ|ថូ)"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData {TNumeral.value = v}:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> case match of
         "នាក់" -> Just . Token Quantity $ quantity (TQuantity.Custom "Person") v
         "ក្បាល" -> Just . Token Quantity $ quantity (TQuantity.Custom "Animal") v
         "ដើម" -> Just . Token Quantity $ quantity (TQuantity.Custom "Tree") v
         "ទង" -> Just . Token Quantity $ quantity (TQuantity.Custom "Flower") v
         "ខ្នង" -> Just . Token Quantity $ quantity (TQuantity.Custom "Building") v
         "មុខ" -> Just . Token Quantity $ quantity (TQuantity.Custom "Food/things") v
         "ចាន" -> Just . Token Quantity $ quantity TQuantity.Bowl v
         "ពែង" -> Just . Token Quantity $ quantity TQuantity.Cup v
         "កែវ" -> Just . Token Quantity $ quantity TQuantity.Cup v
         "ថូ" -> Just . Token Quantity $ quantity TQuantity.Pint v
         _ -> Nothing
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleNumeralUnits
  , ruleQuantityOfProduct
  ]
