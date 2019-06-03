-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Quantity.RO.Rules
  ( rules
  ) where

import Data.String
import Prelude

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (isPositive)
import Duckling.Numeral.Types (NumeralData(..))
import Duckling.Quantity.Helpers
import Duckling.Regex.Types
import Duckling.Types
import qualified Duckling.Numeral.Types as TNumeral
import qualified Duckling.Quantity.Types as TQuantity

ruleNumeralUnits :: Rule
ruleNumeralUnits = Rule
  { name = "<number> <units>"
  , pattern =
    [ Predicate isPositive
    , regex "(de )?livr(a|e|ă)"
    ]
  , prod = \case
      (Token Numeral NumeralData {TNumeral.value = v}:_) ->
        Just . Token Quantity $ quantity TQuantity.Pound v
      _ -> Nothing
  }

ruleQuantityOfProduct :: Rule
ruleQuantityOfProduct = Rule
  { name = "<quantity> of product"
  , pattern =
    [ dimension Quantity
    , regex "de (carne|can[aă]|zah[aă]r|mamaliga)"
    ]
  , prod = \case
      (Token Quantity qd:
       Token RegexMatch (GroupMatch (product:_)):
       _) -> Just . Token Quantity $ withProduct product qd
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleNumeralUnits
  , ruleQuantityOfProduct
  ]
