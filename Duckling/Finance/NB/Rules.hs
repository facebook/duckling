-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Finance.NB.Rules
  ( rules ) where

import Data.Maybe
import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Finance.Helpers
import Duckling.Finance.Types (Currency (..), FinanceData (..))
import qualified Duckling.Finance.Types as TFinance
import Duckling.Number.Types (NumberData (..))
import qualified Duckling.Number.Types as TNumber
import Duckling.Types

ruleIntersectAndNumber :: Rule
ruleIntersectAndNumber = Rule
  { name = "intersect (and number)"
  , pattern =
    [ financeWith TFinance.value isJust
    , regex "og"
    , dimension Numeral
    ]
  , prod = \tokens -> case tokens of
      (Token Finance fd:
       _:
       Token Numeral (NumberData {TNumber.value = c}):
       _) -> Just . Token Finance $ withCents c fd
      _ -> Nothing
  }

ruleAboutAmountofmoney :: Rule
ruleAboutAmountofmoney = Rule
  { name = "about <amount-of-money>"
  , pattern =
    [ regex "omtrent|cirka|rundt|ca"
    , financeWith TFinance.value isJust
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> Just token
      _ -> Nothing
  }

ruleCent :: Rule
ruleCent = Rule
  { name = "cent"
  , pattern =
    [ regex "cents?|penn(y|ies)|(\x00f8)re"
    ]
  , prod = \_ -> Just . Token Finance $ currencyOnly Cent
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

ruleIntersectXCentsWithAnd :: Rule
ruleIntersectXCentsWithAnd = Rule
  { name = "intersect (X cents) with and"
  , pattern =
    [ financeWith TFinance.value isJust
    , regex "og"
    , financeWith TFinance.currency (== Cent)
    ]
  , prod = \tokens -> case tokens of
      (Token Finance fd:
       _:
       Token Finance (FinanceData {TFinance.value = Just c}):
       _) -> Just . Token Finance $ withCents c fd
      _ -> Nothing
  }

ruleNok :: Rule
ruleNok = Rule
  { name = "NOK"
  , pattern =
    [ regex "kr(oner)?"
    ]
  , prod = \_ -> Just . Token Finance $ currencyOnly NOK
  }

rulePound :: Rule
rulePound = Rule
  { name = "£"
  , pattern =
    [ regex "pund?"
    ]
  , prod = \_ -> Just . Token Finance $ currencyOnly Pound
  }

ruleIntersectAndXCents :: Rule
ruleIntersectAndXCents = Rule
  { name = "intersect (and X cents)"
  , pattern =
    [ financeWith TFinance.value isJust
    , regex "og"
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

ruleDirham :: Rule
ruleDirham = Rule
  { name = "AED"
  , pattern =
    [ regex "dirhams?"
    ]
  , prod = \_ -> Just . Token Finance $ currencyOnly AED
  }

rules :: [Rule]
rules =
  [ ruleAboutAmountofmoney
  , ruleCent
  , ruleDirham
  , ruleIntersect
  , ruleIntersectAndNumber
  , ruleIntersectAndXCents
  , ruleIntersectXCents
  , ruleIntersectXCentsWithAnd
  , ruleNok
  , rulePound
  ]
