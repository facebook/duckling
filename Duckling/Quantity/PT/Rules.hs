-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Quantity.PT.Rules
  ( rules ) where

import qualified Data.Text as Text
import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Numeral.Types (NumeralData (..))
import qualified Duckling.Numeral.Types as TNumeral
import Duckling.Quantity.Helpers
import qualified Duckling.Quantity.Types as TQuantity
import Duckling.Regex.Types
import Duckling.Types

ruleNumeralUnits :: Rule
ruleNumeralUnits = Rule
  { name = "<number> <units>"
  , pattern =
    [ dimension Numeral
    , regex "(libra|copo)s?"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData {TNumeral.value = v}:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> case Text.toLower match of
         "copo"   -> Just . Token Quantity $ quantity TQuantity.Cup v
         "libra"  -> Just . Token Quantity $ quantity TQuantity.Pound v
         _        -> Nothing
      _ -> Nothing
  }

ruleQuantityOfProduct :: Rule
ruleQuantityOfProduct = Rule
  { name = "<quantity> of product"
  , pattern =
    [ dimension Quantity
    , regex "de (caf(e|é)|a(ç|c)ucar)"
    ]
  , prod = \tokens -> case tokens of
      (Token Quantity qd:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> Just . Token Quantity $ withProduct match qd
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleNumeralUnits
  , ruleQuantityOfProduct
  ]
