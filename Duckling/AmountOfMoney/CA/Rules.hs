-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.CA.Rules
  ( rules
  ) where

import Data.Maybe
import Data.String
import Prelude

import Duckling.AmountOfMoney.Helpers
import Duckling.AmountOfMoney.Types (Currency(..), AmountOfMoneyData (..))
import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (isNatural, isPositive)
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Types
import qualified Duckling.AmountOfMoney.Types as TAmountOfMoney
import qualified Duckling.Numeral.Types as TNumeral

ruleUnitAmount :: Rule
ruleUnitAmount = Rule
  { name = "<unit> <amount>"
  , pattern =
    [ Predicate isCurrencyOnly
    , Predicate isPositive
    ]
  , prod = \case
      (Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.currency = c}:
       Token Numeral NumeralData{TNumeral.value = v}:
       _) -> Just $ Token AmountOfMoney $ withValue v $ currencyOnly c
      _ -> Nothing
  }

ruleDollar :: Rule
ruleDollar = Rule
  { name = "dollar"
  , pattern =
    [ regex "d(ò|o|ó)lar(s)?"
    ]
  , prod = \_ -> Just $ Token AmountOfMoney $ currencyOnly Dollar
  }

ruleCent :: Rule
ruleCent = Rule
  { name = "cent"
  , pattern =
    [ regex "c(e|è)ntims?"
    ]
  , prod = \_ -> Just $ Token AmountOfMoney $ currencyOnly Cent
  }

rulePounds :: Rule
rulePounds = Rule
  { name = "£"
  , pattern =
    [ regex "(lliure|lliura)s?"
    ]
  , prod = \_ -> Just $ Token AmountOfMoney $ currencyOnly Pound
  }

ruleIntersectAndNumeral :: Rule
ruleIntersectAndNumeral = Rule
  { name = "intersect (and number)"
  , pattern =
    [ Predicate isWithoutCents
    , regex "i"
    , Predicate isNatural
    ]
  , prod = \case
      (Token AmountOfMoney fd:
       _:
       Token Numeral NumeralData{TNumeral.value = c}:
       _) -> Just $ Token AmountOfMoney $ withCents c fd
      _ -> Nothing
  }

ruleIntersectXCents :: Rule
ruleIntersectXCents = Rule
  { name = "intersect (X cents)"
  , pattern =
    [ Predicate isWithoutCents
    , Predicate isCents
    ]
  , prod = \case
      (Token AmountOfMoney fd:
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just c}:
       _) -> Just $ Token AmountOfMoney $ withCents c fd
      _ -> Nothing
  }

ruleIntersectAndXCents :: Rule
ruleIntersectAndXCents = Rule
  { name = "intersect (and X cents)"
  , pattern =
    [ Predicate isWithoutCents
    , regex "i"
    , Predicate isCents
    ]
  , prod = \case
      (Token AmountOfMoney fd:
       _:
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just c}:
       _) -> Just $ Token AmountOfMoney $ withCents c fd
      _ -> Nothing
  }

ruleIntersect :: Rule
ruleIntersect = Rule
  { name = "intersect"
  , pattern =
    [ Predicate isWithoutCents
    , Predicate isNatural
    ]
  , prod = \case
      (Token AmountOfMoney fd:
       Token Numeral NumeralData{TNumeral.value = c}:
       _) -> Just $ Token AmountOfMoney $ withCents c fd
      _ -> Nothing
  }

ruleIntervalBetweenNumeral :: Rule
ruleIntervalBetweenNumeral = Rule
  { name = "between|from <numeral> to|and <amount-of-money>"
  , pattern =
    [ regex "des de|des d'|entre"
    , Predicate isPositive
    , regex "fins a|i"
    , Predicate isSimpleAmountOfMoney
    ]
  , prod = \case
      (_:
       Token Numeral NumeralData{TNumeral.value = from}:
       _:
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just to,
                                             TAmountOfMoney.currency = c}:
       _) | from < to ->
        Just $ Token AmountOfMoney $ withInterval (from, to) $ currencyOnly c
      _ -> Nothing
  }

ruleIntervalBetween :: Rule
ruleIntervalBetween = Rule
  { name = "between|from <amount-of-money> to|and <amount-of-money>"
  , pattern =
    [ regex "des de|des d'|entre"
    , Predicate isSimpleAmountOfMoney
    , regex "fins a|i"
    , Predicate isSimpleAmountOfMoney
    ]
  , prod = \case
      (_:
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just from,
                                             TAmountOfMoney.currency = c1}:
       _:
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just to,
                                             TAmountOfMoney.currency = c2}:
       _) | from < to && c1 == c2 ->
        Just $ Token AmountOfMoney $ withInterval (from, to) $ currencyOnly c1
      _ -> Nothing
  }

ruleIntervalNumeralDash :: Rule
ruleIntervalNumeralDash = Rule
  { name = "<numeral> - <amount-of-money>"
  , pattern =
    [ Predicate isNatural
    , regex "-"
    , Predicate isSimpleAmountOfMoney
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = from}:
       _:
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just to,
                                             TAmountOfMoney.currency = c}:
       _) | from < to->
         Just $ Token AmountOfMoney $ withInterval (from, to) $ currencyOnly c
      _ -> Nothing
  }

ruleIntervalDash :: Rule
ruleIntervalDash = Rule
  { name = "<amount-of-money> - <amount-of-money>"
  , pattern =
    [ Predicate isSimpleAmountOfMoney
    , regex "-"
    , Predicate isSimpleAmountOfMoney
    ]
  , prod = \case
      (Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just from,
                                             TAmountOfMoney.currency = c1}:
       _:
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just to,
                                             TAmountOfMoney.currency = c2}:
       _) | from < to && c1 == c2 ->
        Just $ Token AmountOfMoney $ withInterval (from, to) $ currencyOnly c1
      _ -> Nothing
  }

ruleIntervalMax :: Rule
ruleIntervalMax = Rule
  { name = "less/no more than <amount-of-money>"
  , pattern =
    [ regex "menys de|no m(e|é)s de|no m(e|é)s d'|menys d'"
    , Predicate isSimpleAmountOfMoney
    ]
  , prod = \case
      (_:
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just to,
                                             TAmountOfMoney.currency = c}:
       _) -> Just $ Token AmountOfMoney $ withMax to $ currencyOnly c
      _ -> Nothing
  }

ruleIntervalMin :: Rule
ruleIntervalMin = Rule
  { name = "no less/more than <amount-of-money>"
  , pattern =
    [ regex "m(e|é)s de|no menys de|m(e|é)s d'|no menys d'"
    , Predicate isSimpleAmountOfMoney
    ]
  , prod = \case
      (_:
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just to,
                                             TAmountOfMoney.currency = c}:
       _) -> Just $ Token AmountOfMoney $ withMin to $ currencyOnly c
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleUnitAmount
  , ruleCent
  , ruleDollar
  , ruleIntersect
  , ruleIntersectAndNumeral
  , ruleIntersectAndXCents
  , ruleIntersectXCents
  , rulePounds
  , ruleIntervalBetweenNumeral
  , ruleIntervalBetween
  , ruleIntervalNumeralDash
  , ruleIntervalDash
  , ruleIntervalMax
  , ruleIntervalMin
  ]
