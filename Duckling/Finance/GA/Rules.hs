-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Finance.GA.Rules
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

ruleDollar :: Rule
ruleDollar = Rule
  { name = "$"
  , pattern =
    [ regex "n?dh?oll?ai?rs?"
    ]
  , prod = \_ -> Just . Token Finance $ currencyOnly Dollar
  }

ruleCent :: Rule
ruleCent = Rule
  { name = "cent"
  , pattern =
    [ regex "cents?|g?ch?eint(eanna)?"
    ]
  , prod = \_ -> Just . Token Finance $ currencyOnly Cent
  }

ruleThartArAmountofmoney :: Rule
ruleThartArAmountofmoney = Rule
  { name = "thart ar <amount-of-money>"
  , pattern =
    [ regex "thart( ar)?|beagnach|breis (is|agus)"
    , financeWith TFinance.value isJust
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> Just token
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

rulePounds :: Rule
rulePounds = Rule
  { name = "£"
  , pattern =
    [ regex "pounds?|b?ph?unt"
    ]
  , prod = \_ -> Just . Token Finance $ currencyOnly Pound
  }

ruleIntersectAndXCents :: Rule
ruleIntersectAndXCents = Rule
  { name = "intersect (and X cents)"
  , pattern =
    [ financeWith TFinance.value isJust
    , regex "agus|is"
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
    , dimension DNumber
    ]
  , prod = \tokens -> case tokens of
      (Token Finance fd:
       Token DNumber (NumberData {TNumber.value = c}):
       _) -> Just . Token Finance $ withCents c fd
      _ -> Nothing
  }

ruleInr :: Rule
ruleInr = Rule
  { name = "INR"
  , pattern =
    [ regex "r(\x00fa|u)pa(\x00ed|i)"
    ]
  , prod = \_ -> Just . Token Finance $ currencyOnly INR
  }

ruleIntersectAgusNumber :: Rule
ruleIntersectAgusNumber = Rule
  { name = "intersect (agus number)"
  , pattern =
    [ financeWith TFinance.value isJust
    , regex "agus|is"
    , dimension DNumber
    ]
  , prod = \tokens -> case tokens of
      (Token Finance fd:_:Token DNumber (NumberData {TNumber.value = c}):_) ->
        Just . Token Finance $ withCents c fd
      _ -> Nothing
  }

ruleAmountofmoneyGlan :: Rule
ruleAmountofmoneyGlan = Rule
  { name = "<amount-of-money> glan"
  , pattern =
    [ financeWith TFinance.value isJust
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
  , ruleIntersectAgusNumber
  , ruleIntersectAndXCents
  , ruleIntersectXCents
  , rulePounds
  , ruleThartArAmountofmoney
  ]
