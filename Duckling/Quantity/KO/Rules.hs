-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Quantity.KO.Rules
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
    [ dimension Quantity
    , regex "의 (삼겹살|콜라)"
    ]
  , prod = \tokens -> case tokens of
      (Token Quantity qd:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> Just . Token Quantity $ withProduct match qd
      _ -> Nothing
  }

ruleQuantityOfProduct2 :: Rule
ruleQuantityOfProduct2 = Rule
  { name = "<quantity> of product"
  , pattern =
    [ regex "(삼겹살|콜라)"
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
  { name = "<number> <units>"
  , pattern =
    [ dimension Numeral
    , regex "(개|판|그(램|람)|근|파운(드|즈)|접1시|그릇|컵)"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData {TNumeral.value = v}:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> case match of
         "개"             -> Just . Token Quantity $ quantity TQuantity.Unnamed v
         "판"             -> Just . Token Quantity $ quantity (TQuantity.Custom "판") v
         "근"             -> Just . Token Quantity $ quantity (TQuantity.Custom "근") v
         "그램"       -> Just . Token Quantity $ quantity TQuantity.Gram v
         "그람"       -> Just . Token Quantity $ quantity TQuantity.Gram v
         "파운드" -> Just . Token Quantity $ quantity TQuantity.Pound v
         "파운즈" -> Just . Token Quantity $ quantity TQuantity.Pound v
         "접1시"      -> Just . Token Quantity $ quantity TQuantity.Dish v
         "그릇"       -> Just . Token Quantity $ quantity TQuantity.Bowl v
         "컵"             -> Just . Token Quantity $ quantity TQuantity.Cup v
         _                    -> Nothing
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleNumeralUnits
  , ruleQuantityOfProduct
  , ruleQuantityOfProduct2
  ]
