-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.ES.Rules
  ( rules ) where

import Data.Maybe
import Prelude
import Data.String

import Duckling.AmountOfMoney.Helpers
import Duckling.AmountOfMoney.Types (Currency(..), AmountOfMoneyData (..))
import qualified Duckling.AmountOfMoney.Types as TAmountOfMoney
import Duckling.Dimensions.Types
import Duckling.Numeral.Types (NumeralData (..))
import qualified Duckling.Numeral.Types as TNumeral
import Duckling.Types

ruleDollar :: Rule
ruleDollar = Rule
  { name = "dollar"
  , pattern =
    [ regex "d(\x00f3|o)lar(es)?"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly Dollar
  }

ruleCent :: Rule
ruleCent = Rule
  { name = "cent"
  , pattern =
    [ regex "centavos?"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly Cent
  }

rulePounds :: Rule
rulePounds = Rule
  { name = "£"
  , pattern =
    [ regex "(pound|libra)s?"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly Pound
  }

ruleIntersectAndNumeral :: Rule
ruleIntersectAndNumeral = Rule
  { name = "intersect (and number)"
  , pattern =
    [ financeWith TAmountOfMoney.value isJust
    , regex "y"
    , dimension Numeral
    ]
  , prod = \tokens -> case tokens of
      (Token AmountOfMoney fd:
       _:
       Token Numeral (NumeralData {TNumeral.value = c}):
       _) -> Just . Token AmountOfMoney $ withCents c fd
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

ruleIntersectAndXCents :: Rule
ruleIntersectAndXCents = Rule
  { name = "intersect (and X cents)"
  , pattern =
    [ financeWith TAmountOfMoney.value isJust
    , regex "y"
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

rules :: [Rule]
rules =
  [ ruleCent
  , ruleDollar
  , ruleIntersect
  , ruleIntersectAndNumeral
  , ruleIntersectAndXCents
  , ruleIntersectXCents
  , rulePounds
  ]
