-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.KO.Rules
  ( rules
  ) where

import Data.Maybe
import Data.String
import Prelude

import Duckling.Dimensions.Types
import Duckling.AmountOfMoney.Helpers
import Duckling.AmountOfMoney.Types (Currency(..), AmountOfMoneyData (..))
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
       _) -> Just . Token AmountOfMoney . withValue v $ currencyOnly c
      _ -> Nothing
  }

ruleAmountofmoneyAbout :: Rule
ruleAmountofmoneyAbout = Rule
  { name = "<amount-of-money> about"
  , pattern =
    [ Predicate isMoneyWithValue
    , regex "정도|쯤"
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> Just token
      _ -> Nothing
  }

ruleAud :: Rule
ruleAud = Rule
  { name = "AUD"
  , pattern =
    [ regex "호주달러"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly AUD
  }

ruleKrw :: Rule
ruleKrw = Rule
  { name = "₩"
  , pattern =
    [ regex "₩|원"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly KRW
  }

ruleAboutAmountofmoney :: Rule
ruleAboutAmountofmoney = Rule
  { name = "about <amount-of-money>"
  , pattern =
    [ regex "약|대충|얼추"
    , Predicate isMoneyWithValue
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> Just token
      _ -> Nothing
  }

ruleCent :: Rule
ruleCent = Rule
  { name = "cent"
  , pattern =
    [ regex "센트"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly Cent
  }

ruleExactlyAmountofmoney :: Rule
ruleExactlyAmountofmoney = Rule
  { name = "exactly <amount-of-money>"
  , pattern =
    [ regex "딱|정확히"
    , Predicate isMoneyWithValue
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> Just token
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

ruleEuro :: Rule
ruleEuro = Rule
  { name = "€"
  , pattern =
    [ regex "€|유로"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly EUR
  }

ruleDollar :: Rule
ruleDollar = Rule
  { name = "$"
  , pattern =
    [ regex "달러|불"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly Dollar
  }

ruleInr :: Rule
ruleInr = Rule
  { name = "INR"
  , pattern =
    [ regex "루피|인도루피"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly INR
  }

rulePounds :: Rule
rulePounds = Rule
  { name = "£"
  , pattern =
    [ regex "파운드|영국파운드"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly Pound
  }

ruleDirham :: Rule
ruleDirham = Rule
  { name = "dirham"
  , pattern =
    [ regex "디르함"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly AED
  }

ruleIntervalBetweenNumeral :: Rule
ruleIntervalBetweenNumeral = Rule
  { name = "between|from <numeral> to|and <amount-of-money>"
  , pattern =
    [ Predicate isPositive
    , regex "에서"
    , Predicate isSimpleAmountOfMoney
    ]
  , prod = \case
      (Token Numeral NumeralData {TNumeral.value = from}:
       _:
       Token AmountOfMoney AmountOfMoneyData {TAmountOfMoney.value = Just to, TAmountOfMoney.currency = c}:
       _) ->
        (Just (Token AmountOfMoney $ withInterval (from, to) $ currencyOnly c))
      _ -> Nothing
  }

ruleIntervalBetween :: Rule
ruleIntervalBetween = Rule
  { name = "between|from <amount-of-money> to|and <amount-of-money>"
  , pattern =
    [ Predicate isSimpleAmountOfMoney
    , regex "부터"
    , Predicate isSimpleAmountOfMoney
    ]
  , prod = \case
      (Token AmountOfMoney AmountOfMoneyData {TAmountOfMoney.value = Just from, TAmountOfMoney.currency = c1}:
       _:
       Token AmountOfMoney AmountOfMoneyData {TAmountOfMoney.value = Just to, TAmountOfMoney.currency = c2}:
       _) | c1 == c2 ->
        (Just (Token AmountOfMoney $ withInterval (from, to) $ currencyOnly c1))
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
      (Token Numeral NumeralData {TNumeral.value = from}:
       _:
       Token AmountOfMoney AmountOfMoneyData {TAmountOfMoney.value = Just to, TAmountOfMoney.currency = c}:
       _) ->
        (Just (Token AmountOfMoney $ withInterval (from, to) $ currencyOnly c))
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
      (Token AmountOfMoney AmountOfMoneyData {TAmountOfMoney.value = Just from, TAmountOfMoney.currency = c1}:
       _:
       Token AmountOfMoney AmountOfMoneyData {TAmountOfMoney.value = Just to, TAmountOfMoney.currency = c2}:
       _) | c1 == c2 ->
        (Just (Token AmountOfMoney $ withInterval (from, to) $ currencyOnly c1))
      _ -> Nothing
  }

ruleIntervalMax :: Rule
ruleIntervalMax = Rule
  { name = "under/less/lower/no more than <amount-of-money>"
  , pattern =
    [ Predicate isSimpleAmountOfMoney
    , regex "미만|이하"
    ]
  , prod = \case
      (Token AmountOfMoney AmountOfMoneyData {TAmountOfMoney.value = Just to, TAmountOfMoney.currency = c}:
       _:
       _) -> (Just (Token AmountOfMoney $ withMax to $ currencyOnly c))
      _ -> Nothing
  }

ruleIntervalMin :: Rule
ruleIntervalMin = Rule
  { name = "over/above/at least/more than <amount-of-money>"
  , pattern =
    [ Predicate isSimpleAmountOfMoney
    , regex "초과|이상"
    ]
  , prod = \case
      (Token AmountOfMoney AmountOfMoneyData {TAmountOfMoney.value = Just to, TAmountOfMoney.currency = c}:
       _:
       _) -> (Just (Token AmountOfMoney $ withMin to $ currencyOnly c))
      _ -> Nothing
  }


rules :: [Rule]
rules =
  [ ruleUnitAmount
  , ruleAboutAmountofmoney
  , ruleAmountofmoneyAbout
  , ruleAud
  , ruleCent
  , ruleDirham
  , ruleDollar
  , ruleEuro
  , ruleExactlyAmountofmoney
  , ruleInr
  , ruleIntersectXCents
  , ruleKrw
  , rulePounds
  , ruleIntervalBetweenNumeral
  , ruleIntervalBetween
  , ruleIntervalNumeralDash
  , ruleIntervalDash
  , ruleIntervalMax
  , ruleIntervalMin
  ]
