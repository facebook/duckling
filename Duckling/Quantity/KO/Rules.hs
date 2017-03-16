-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


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
    , regex "\xc758 (\xc0bc\xacb9\xc0b4|\xcf5c\xb77c)"
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
    [ regex "(\xc0bc\xacb9\xc0b4|\xcf5c\xb77c)"
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
    , regex "(\xac1c|\xd310|\xadf8(\xb7a8|\xb78c)|\xadfc|\xd30c\xc6b4(\xb4dc|\xc988)|\xc8111\xc2dc|\xadf8\xb987|\xcef5)"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData {TNumeral.value = v}:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> case match of
         "\xac1c"             -> Just . Token Quantity $ quantity TQuantity.Unnamed v
         "\xd310"             -> Just . Token Quantity $ quantity (TQuantity.Custom "판") v
         "\xadfc"             -> Just . Token Quantity $ quantity (TQuantity.Custom "근") v
         "\xadf8\xb7a8"       -> Just . Token Quantity $ quantity TQuantity.Gram v
         "\xadf8\xb78c"       -> Just . Token Quantity $ quantity TQuantity.Gram v
         "\xd30c\xc6b4\xb4dc" -> Just . Token Quantity $ quantity TQuantity.Pound v
         "\xd30c\xc6b4\xc988" -> Just . Token Quantity $ quantity TQuantity.Pound v
         "\xc8111\xc2dc"      -> Just . Token Quantity $ quantity TQuantity.Dish v
         "\xadf8\xb987"       -> Just . Token Quantity $ quantity TQuantity.Bowl v
         "\xcef5"             -> Just . Token Quantity $ quantity TQuantity.Cup v
         _                    -> Nothing
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleNumeralUnits
  , ruleQuantityOfProduct
  , ruleQuantityOfProduct2
  ]
