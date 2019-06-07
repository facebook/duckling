-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Quantity.FR.Rules
  ( rules
  ) where

import Data.String
import Prelude
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Quantity.Helpers
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Regex.Types
import Duckling.Types
import qualified Duckling.Numeral.Types as TNumeral
import qualified Duckling.Quantity.Types as TQuantity

ruleNumeralUnits :: Rule
ruleNumeralUnits = Rule
  { name = "<number> <units>"
  , pattern =
    [ dimension Numeral
    , regex "(tasses?|cuill?(e|è)res? (a|à) soupe?)"
    ]
  , prod = \case
      (Token Numeral NumeralData {TNumeral.value = v}:
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
    , regex "de (caf(e|é)|sucre)"
    ]
  , prod = \case
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
