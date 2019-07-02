-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.HR.Rules
  ( rules
  ) where

import Data.Maybe
import Data.String
import Data.Text (Text)
import Prelude
import qualified Data.Text as Text

import Duckling.AmountOfMoney.Helpers
import Duckling.AmountOfMoney.Types (Currency(..), AmountOfMoneyData (..))
import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (isNatural, isPositive)
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Regex.Types
import Duckling.Types
import qualified Duckling.AmountOfMoney.Types as TAmountOfMoney
import qualified Duckling.Numeral.Types as TNumeral

ruleACurrency :: Rule
ruleACurrency = Rule
  { name = "a <currency>"
  , pattern =
    [ regex "jed(an|na|no)"
    , Predicate isCurrencyOnly
    ]
  , prod = \tokens -> case tokens of
      (_:
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.currency = c}:
       _) -> Just . Token AmountOfMoney $ withValue 1 $ currencyOnly c
      _ -> Nothing
  }

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
       _) -> Just . Token AmountOfMoney . withValue v $ currencyOnly c
      _ -> Nothing
  }

ruleIntersectAndNumber :: Rule
ruleIntersectAndNumber = Rule
  { name = "intersect (and number)"
  , pattern =
    [ Predicate isWithoutCents
    , regex "i"
    , Predicate isNatural
    ]
  , prod = \tokens -> case tokens of
      (Token AmountOfMoney fd:
       _:
       Token Numeral NumeralData{TNumeral.value = c}:
       _) -> Just . Token AmountOfMoney $ withCents c fd
      _ -> Nothing
  }

ruleSar :: Rule
ruleSar = Rule
  { name = "SAR"
  , pattern =
    [ regex "saudijskirijal|saudi rijal?"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly SAR
  }

ruleDollar :: Rule
ruleDollar = Rule
  { name = "$"
  , pattern =
    [ regex "dolar(a|i|e)?"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly Dollar
  }

ruleCent :: Rule
ruleCent = Rule
  { name = "cent"
  , pattern =
    [ regex "cent(i|a)?|penij(i|a)?|c|¢|lp|lip(a|e)"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly Cent
  }

ruleIntersectIXLipa :: Rule
ruleIntersectIXLipa = Rule
  { name = "intersect (i X lipa)"
  , pattern =
    [ Predicate isWithoutCents
    , regex "i"
    , Predicate isCents
    ]
  , prod = \tokens -> case tokens of
      (Token AmountOfMoney fd:
       _:
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just c}:
       _) -> Just . Token AmountOfMoney $ withCents c fd
      _ -> Nothing
  }

ruleIntersectXCents :: Rule
ruleIntersectXCents = Rule
  { name = "intersect (X cents)"
  , pattern =
    [ Predicate isWithoutCents
    , Predicate isCents
    ]
  , prod = \tokens -> case tokens of
      (Token AmountOfMoney fd:
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just c}:
       _) -> Just . Token AmountOfMoney $ withCents c fd
      _ -> Nothing
  }

rulePound :: Rule
rulePound = Rule
  { name = "£"
  , pattern =
    [ regex "funt(a|e|i)?"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly Pound
  }

ruleHrk :: Rule
ruleHrk = Rule
  { name = "HRK"
  , pattern =
    [ regex "kn|(hrvatsk(a|ih|e) )?kun(a|e)"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly HRK
  }

ruleIntersect :: Rule
ruleIntersect = Rule
  { name = "intersect"
  , pattern =
    [ Predicate isWithoutCents
    , Predicate isNatural
    ]
  , prod = \tokens -> case tokens of
      (Token AmountOfMoney fd:
       Token Numeral NumeralData{TNumeral.value = c}:
       _) -> Just . Token AmountOfMoney $ withCents c fd
      _ -> Nothing
  }

ruleOtherPounds :: Rule
ruleOtherPounds = Rule
  { name = "other pounds"
  , pattern =
    [ regex "(egipatska|libanonska) ?funta"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "egipatska"  -> Just . Token AmountOfMoney $ currencyOnly EGP
        "libanonska" -> Just . Token AmountOfMoney $ currencyOnly LBP
        _            -> Nothing
      _ -> Nothing
  }

ruleInr :: Rule
ruleInr = Rule
  { name = "INR"
  , pattern =
    [ regex "rupija?"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly INR
  }

ruleKwd :: Rule
ruleKwd = Rule
  { name = "KWD"
  , pattern =
    [ regex "kuvajtski ?dinar"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly KWD
  }

ruleQar :: Rule
ruleQar = Rule
  { name = "QAR"
  , pattern =
    [ regex "katarski(i| )rijal"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly QAR
  }

ruleAed :: Rule
ruleAed = Rule
  { name = "AED"
  , pattern =
    [ regex "drahma?"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly AED
  }

rulePrecision :: Rule
rulePrecision = Rule
  { name = "about|exactly <amount-of-money>"
  , pattern =
    [ regex "oko|otprilike|u blizini|skoro|približno"
    , Predicate isMoneyWithValue
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> Just token
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
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = from}:
       _:
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just to,
                  TAmountOfMoney.currency = c}:
       _) | from < to->
         Just . Token AmountOfMoney . withInterval (from, to) $ currencyOnly c
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
  , prod = \tokens -> case tokens of
      (Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just from,
                  TAmountOfMoney.currency = c1}:
       _:
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just to,
                  TAmountOfMoney.currency = c2}:
       _) | from < to && c1 == c2 ->
        Just . Token AmountOfMoney . withInterval (from, to) $ currencyOnly c1
      _ -> Nothing
  }

ruleIntervalBetweenNumeral1 :: Rule
ruleIntervalBetweenNumeral1 = Rule
  { name = "between|from <numeral> to|and <amount-of-money>"
  , pattern =
    [ regex "od|otprilike|približno"
    , Predicate isPositive
    , regex "do"
    , Predicate isSimpleAmountOfMoney
    ]
  , prod = \tokens -> case tokens of
      (_:
       Token Numeral NumeralData{TNumeral.value = from}:
       _:
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just to,
                  TAmountOfMoney.currency = c}:
       _) | from < to ->
        Just . Token AmountOfMoney . withInterval (from, to) $ currencyOnly c
      _ -> Nothing
  }

ruleIntervalBetweenNumeral2 :: Rule
ruleIntervalBetweenNumeral2 = Rule
  { name = "between|from <numeral> to|and <amount-of-money>"
  , pattern =
    [ regex "izmedju"
    , Predicate isPositive
    , regex "i"
    , Predicate isSimpleAmountOfMoney
    ]
  , prod = \tokens -> case tokens of
      (_:
       Token Numeral NumeralData{TNumeral.value = from}:
       _:
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just to,
                  TAmountOfMoney.currency = c}:
       _) | from < to ->
        Just . Token AmountOfMoney . withInterval (from, to) $ currencyOnly c
      _ -> Nothing
  }

ruleIntervalBetween1 :: Rule
ruleIntervalBetween1 = Rule
  { name = "between|from <amount-of-money> to|and <amount-of-money>"
  , pattern =
    [ regex "od|otprilike"
    , Predicate isSimpleAmountOfMoney
    , regex "do"
    , Predicate isSimpleAmountOfMoney
    ]
  , prod = \tokens -> case tokens of
      (_:
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just from,
                  TAmountOfMoney.currency = c1}:
       _:
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just to,
                  TAmountOfMoney.currency = c2}:
       _) | from < to && c1 == c2 ->
        Just . Token AmountOfMoney . withInterval (from, to) $ currencyOnly c1
      _ -> Nothing
  }

ruleIntervalBetween2 :: Rule
ruleIntervalBetween2 = Rule
  { name = "between|from <amount-of-money> to|and <amount-of-money>"
  , pattern =
    [ regex "izmedju"
    , Predicate isSimpleAmountOfMoney
    , regex "i"
    , Predicate isSimpleAmountOfMoney
    ]
  , prod = \tokens -> case tokens of
      (_:
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just from,
                  TAmountOfMoney.currency = c1}:
       _:
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just to,
                  TAmountOfMoney.currency = c2}:
       _) | from < to && c1 == c2 ->
        Just . Token AmountOfMoney . withInterval (from, to) $ currencyOnly c1
      _ -> Nothing
  }

ruleIntervalMin :: Rule
ruleIntervalMin = Rule
  { name = "over/above/at least/more than <amount-of-money>"
  , pattern =
    [ regex "više od|najmanje|preko|iznad"
    , Predicate isSimpleAmountOfMoney
    ]
  , prod = \tokens -> case tokens of
      (_:
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just to,
                  TAmountOfMoney.currency = c}:
       _) -> Just . Token AmountOfMoney . withMin to $ currencyOnly c
      _ -> Nothing
  }

ruleIntervalMax :: Rule
ruleIntervalMax = Rule
  { name = "under/less/lower/no more than <amount-of-money>"
  , pattern =
    [ regex "ispod|manje od"
    , Predicate isSimpleAmountOfMoney
    ]
  , prod = \tokens -> case tokens of
      (_:
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just to,
                  TAmountOfMoney.currency = c}:
       _) -> Just . Token AmountOfMoney . withMax to $ currencyOnly c
      _ -> Nothing
  }


rules :: [Rule]
rules =
  [ ruleACurrency
  , ruleUnitAmount
  , ruleDollar
  , rulePound
  , ruleOtherPounds
  , ruleAed
  , ruleCent
  , ruleHrk
  , ruleInr
  , ruleIntersect
  , ruleIntersectAndNumber
  , ruleIntersectIXLipa
  , ruleIntersectXCents
  , ruleKwd
  , ruleQar
  , ruleSar
  , rulePrecision
  , ruleIntervalNumeralDash
  , ruleIntervalDash
  , ruleIntervalBetweenNumeral1
  , ruleIntervalBetweenNumeral2
  , ruleIntervalBetween1
  , ruleIntervalBetween2
  , ruleIntervalMin
  , ruleIntervalMax
  ]
