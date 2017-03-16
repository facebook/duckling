-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.NB.Rules
  ( rules ) where

import Data.Maybe
import Prelude
import Data.String

import Duckling.AmountOfMoney.Helpers
import Duckling.AmountOfMoney.Types (Currency (..), AmountOfMoneyData (..))
import qualified Duckling.AmountOfMoney.Types as TAmountOfMoney
import Duckling.Dimensions.Types
import Duckling.Numeral.Types (NumeralData (..))
import qualified Duckling.Numeral.Types as TNumeral
import Duckling.Types

ruleIntersectAndNumeral :: Rule
ruleIntersectAndNumeral = Rule
  { name = "intersect (and number)"
  , pattern =
    [ financeWith TAmountOfMoney.value isJust
    , regex "og"
    , dimension Numeral
    ]
  , prod = \tokens -> case tokens of
      (Token AmountOfMoney fd:
       _:
       Token Numeral (NumeralData {TNumeral.value = c}):
       _) -> Just . Token AmountOfMoney $ withCents c fd
      _ -> Nothing
  }

ruleAboutAmountofmoney :: Rule
ruleAboutAmountofmoney = Rule
  { name = "about <amount-of-money>"
  , pattern =
    [ regex "omtrent|cirka|rundt|ca"
    , financeWith TAmountOfMoney.value isJust
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
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly Cent
  }

ruleIntersectXCents :: Rule
ruleIntersectXCents = Rule
  { name = "intersect (X cents)"
  , pattern =
    [ financeWith TAmountOfMoney.value isJust
    , financeWith TAmountOfMoney.currency (== Cent)
    ]
  , prod = \tokens -> case tokens of
      (Token AmountOfMoney fd:
       Token AmountOfMoney (AmountOfMoneyData {TAmountOfMoney.value = Just c}):
       _) -> Just . Token AmountOfMoney $ withCents c fd
      _ -> Nothing
  }

ruleIntersectXCentsWithAnd :: Rule
ruleIntersectXCentsWithAnd = Rule
  { name = "intersect (X cents) with and"
  , pattern =
    [ financeWith TAmountOfMoney.value isJust
    , regex "og"
    , financeWith TAmountOfMoney.currency (== Cent)
    ]
  , prod = \tokens -> case tokens of
      (Token AmountOfMoney fd:
       _:
       Token AmountOfMoney (AmountOfMoneyData {TAmountOfMoney.value = Just c}):
       _) -> Just . Token AmountOfMoney $ withCents c fd
      _ -> Nothing
  }

ruleNok :: Rule
ruleNok = Rule
  { name = "NOK"
  , pattern =
    [ regex "kr(oner)?"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly NOK
  }

rulePound :: Rule
rulePound = Rule
  { name = "£"
  , pattern =
    [ regex "pund?"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly Pound
  }

ruleIntersectAndXCents :: Rule
ruleIntersectAndXCents = Rule
  { name = "intersect (and X cents)"
  , pattern =
    [ financeWith TAmountOfMoney.value isJust
    , regex "og"
    , financeWith TAmountOfMoney.currency (== Cent)
    ]
  , prod = \tokens -> case tokens of
      (Token AmountOfMoney fd:
       _:
       Token AmountOfMoney (AmountOfMoneyData {TAmountOfMoney.value = Just c}):
       _) -> Just . Token AmountOfMoney $ withCents c fd
      _ -> Nothing
  }

ruleIntersect :: Rule
ruleIntersect = Rule
  { name = "intersect"
  , pattern =
    [ financeWith TAmountOfMoney.value isJust
    , dimension Numeral
    ]
  , prod = \tokens -> case tokens of
      (Token AmountOfMoney fd:
       Token Numeral (NumeralData {TNumeral.value = c}):
       _) -> Just . Token AmountOfMoney $ withCents c fd
      _ -> Nothing
  }

ruleDirham :: Rule
ruleDirham = Rule
  { name = "AED"
  , pattern =
    [ regex "dirhams?"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly AED
  }

rules :: [Rule]
rules =
  [ ruleAboutAmountofmoney
  , ruleCent
  , ruleDirham
  , ruleIntersect
  , ruleIntersectAndNumeral
  , ruleIntersectAndXCents
  , ruleIntersectXCents
  , ruleIntersectXCentsWithAnd
  , ruleNok
  , rulePound
  ]
