-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.GA.Rules
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

ruleDollar :: Rule
ruleDollar = Rule
  { name = "$"
  , pattern =
    [ regex "n?dh?oll?ai?rs?"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly Dollar
  }

ruleCent :: Rule
ruleCent = Rule
  { name = "cent"
  , pattern =
    [ regex "cents?|g?ch?eint(eanna)?"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly Cent
  }

ruleThartArAmountofmoney :: Rule
ruleThartArAmountofmoney = Rule
  { name = "thart ar <amount-of-money>"
  , pattern =
    [ regex "thart( ar)?|beagnach|breis (is|agus)"
    , financeWith TAmountOfMoney.value isJust
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> Just token
      _ -> Nothing
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

rulePounds :: Rule
rulePounds = Rule
  { name = "£"
  , pattern =
    [ regex "pounds?|b?ph?unt"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly Pound
  }

ruleIntersectAndXCents :: Rule
ruleIntersectAndXCents = Rule
  { name = "intersect (and X cents)"
  , pattern =
    [ financeWith TAmountOfMoney.value isJust
    , regex "agus|is"
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

ruleInr :: Rule
ruleInr = Rule
  { name = "INR"
  , pattern =
    [ regex "r(\x00fa|u)pa(\x00ed|i)"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly INR
  }

ruleIntersectAgusNumeral :: Rule
ruleIntersectAgusNumeral = Rule
  { name = "intersect (agus number)"
  , pattern =
    [ financeWith TAmountOfMoney.value isJust
    , regex "agus|is"
    , dimension Numeral
    ]
  , prod = \tokens -> case tokens of
      (Token AmountOfMoney fd:
       _:
       Token Numeral (NumeralData {TNumeral.value = c}):
       _) -> Just . Token AmountOfMoney $ withCents c fd
      _ -> Nothing
  }

ruleAmountofmoneyGlan :: Rule
ruleAmountofmoneyGlan = Rule
  { name = "<amount-of-money> glan"
  , pattern =
    [ financeWith TAmountOfMoney.value isJust
    , regex "glan|baileach|(go )?d(\x00ed|i)reach"
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> Just token
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleAmountofmoneyGlan
  , ruleCent
  , ruleDollar
  , ruleInr
  , ruleIntersect
  , ruleIntersectAgusNumeral
  , ruleIntersectAndXCents
  , ruleIntersectXCents
  , rulePounds
  , ruleThartArAmountofmoney
  ]
