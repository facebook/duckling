-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Quantity.HR.Rules
  ( rules ) where

import Data.String
import Prelude
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Quantity.Helpers
import Duckling.Regex.Types
import Duckling.Types
import qualified Duckling.Numeral.Types as TNumeral
import qualified Duckling.Quantity.Types as TQuantity

ruleNumberUnits :: Rule
ruleNumberUnits = Rule
  { name = "<number> <units>"
  , pattern =
    [ dimension Numeral
    , regex "k(il(o|e|a))?(g(rama?)?)?"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData {TNumeral.value = v}:_) ->
        Just . Token Quantity $ quantity TQuantity.Gram (1000 * v)
      _ -> Nothing
  }

ruleQuantityProduct :: Rule
ruleQuantityProduct = Rule
  { name = "<quantity> product"
  , pattern =
    [ dimension Quantity
    , regex "(mes(o|a)|soli?)"
    ]
  , prod = \tokens -> case tokens of
      (Token Quantity qd:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> case Text.toLower match of
         "meso" -> Just . Token Quantity $ withProduct "meso" qd
         "mesa" -> Just . Token Quantity $ withProduct "meso" qd
         "sol" -> Just . Token Quantity $ withProduct "sol" qd
         "soli" -> Just . Token Quantity $ withProduct "sol" qd
         _      -> Nothing
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleNumberUnits
  , ruleQuantityProduct
  ]
