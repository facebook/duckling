-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Finance.ES.Rules
  ( rules ) where

import Data.Maybe
import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Finance.Helpers
import Duckling.Finance.Types (Currency(..), FinanceData (..))
import qualified Duckling.Finance.Types as TFinance
import Duckling.Number.Types (NumberData (..))
import qualified Duckling.Number.Types as TNumber
import Duckling.Types

ruleDollar :: Rule
ruleDollar = Rule
  { name = "dollar"
  , pattern =
    [ regex "d(\x00f3|o)lar(es)?"
    ]
  , prod = \_ -> Just . Token Finance $ currencyOnly Dollar
  }

ruleCent :: Rule
ruleCent = Rule
  { name = "cent"
  , pattern =
    [ regex "centavos?"
    ]
  , prod = \_ -> Just . Token Finance $ currencyOnly Cent
  }

rulePounds :: Rule
rulePounds = Rule
  { name = "£"
  , pattern =
    [ regex "(pound|libra)s?"
    ]
  , prod = \_ -> Just . Token Finance $ currencyOnly Pound
  }

ruleIntersectAndNumber :: Rule
ruleIntersectAndNumber = Rule
  { name = "intersect (and number)"
  , pattern =
    [ financeWith TFinance.value isJust
    , regex "y"
    , dimension Numeral
    ]
  , prod = \tokens -> case tokens of
      (Token Finance fd:
       _:
       Token Numeral (NumberData {TNumber.value = c}):
       _) -> Just . Token Finance $ withCents c fd
      _ -> Nothing
  }

ruleIntersectXCents :: Rule
ruleIntersectXCents = Rule
  { name = "intersect (X cents)"
  , pattern =
    [ financeWith TFinance.value isJust
    , financeWith TFinance.currency (== Cent)
    ]
  , prod = \tokens -> case tokens of
      (Token Finance fd:
       Token Finance (FinanceData {TFinance.value = Just c}):
       _) -> Just . Token Finance $ withCents c fd
      _ -> Nothing
  }

ruleIntersectAndXCents :: Rule
ruleIntersectAndXCents = Rule
  { name = "intersect (and X cents)"
  , pattern =
    [ financeWith TFinance.value isJust
    , regex "y"
    , financeWith TFinance.currency (== Cent)
    ]
  , prod = \tokens -> case tokens of
      (Token Finance fd:
       _:
       Token Finance (FinanceData {TFinance.value = Just c}):
       _) -> Just . Token Finance $ withCents c fd
      _ -> Nothing
  }

ruleIntersect :: Rule
ruleIntersect = Rule
  { name = "intersect"
  , pattern =
    [ financeWith TFinance.value isJust
    , dimension Numeral
    ]
  , prod = \tokens -> case tokens of
      (Token Finance fd:
       Token Numeral (NumberData {TNumber.value = c}):
       _) -> Just . Token Finance $ withCents c fd
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleCent
  , ruleDollar
  , ruleIntersect
  , ruleIntersectAndNumber
  , ruleIntersectAndXCents
  , ruleIntersectXCents
  , rulePounds
  ]
