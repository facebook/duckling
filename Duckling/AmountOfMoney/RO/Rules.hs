-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.RO.Rules
  ( rules
  ) where

import Data.Maybe
import Data.String
import Prelude
import qualified Data.Text as Text

import Duckling.AmountOfMoney.Helpers
import Duckling.AmountOfMoney.Types (Currency(..), AmountOfMoneyData (..))
import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (isNatural, isPositive, numberWith)
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Regex.Types
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

ruleAmountDeUnit :: Rule
ruleAmountDeUnit = Rule
  { name = "<amount >= 20> de <unit>"
  , pattern =
    [ numberWith TNumeral.value (>= 20)
    , regex "de"
    , Predicate isCurrencyOnly
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = v}:
       _:
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.currency = c}:
       _) -> Just . Token AmountOfMoney . withValue v $ currencyOnly c
      _ -> Nothing
  }

ruleIntersectAndNumeral :: Rule
ruleIntersectAndNumeral = Rule
  { name = "intersect (and number)"
  , pattern =
    [ Predicate isWithoutCents
    , regex "(s|ș)i"
    , Predicate isNatural
    ]
  , prod = \tokens -> case tokens of
      (Token AmountOfMoney fd:
       _:
       Token Numeral NumeralData{TNumeral.value = c}:
       _) -> Just . Token AmountOfMoney $ withCents c fd
      _ -> Nothing
  }

ruleRiyals :: Rule
ruleRiyals = Rule
  { name = "riyals"
  , pattern =
    [ regex "rial (saudit?|qatarian?)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "saudi"     -> Just . Token AmountOfMoney $ currencyOnly SAR
        "saudit"    -> Just . Token AmountOfMoney $ currencyOnly SAR
        "qataria"   -> Just . Token AmountOfMoney $ currencyOnly QAR
        "qatarian"  -> Just . Token AmountOfMoney $ currencyOnly QAR
        _           -> Nothing
      _ -> Nothing
  }

ruleDollar :: Rule
ruleDollar = Rule
  { name = "$"
  , pattern =
    [ regex "dolari?"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly Dollar
  }

rulePrecisionAmountOfMoney :: Rule
rulePrecisionAmountOfMoney = Rule
  { name = "about/exactly <amount-of-money>"
  , pattern =
    [ regex "exact|cam|aprox(\\.|imativ)?|(aproape|(i|î)n jur)( de)?"
    , Predicate isMoneyWithValue
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> Just token
      _ -> Nothing
  }

ruleCent :: Rule
ruleCent = Rule
  { name = "cent|bani"
  , pattern =
    [ regex "bani?|cen(t|ț)i?|c|¢"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly Cent
  }

ruleRon :: Rule
ruleRon = Rule
  { name = "RON"
  , pattern =
    [ regex "roni|lei"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly RON
  }

ruleIntersectAndXCents :: Rule
ruleIntersectAndXCents = Rule
  { name = "intersect (and X cents)"
  , pattern =
    [ Predicate isWithoutCents
    , regex "(s|ș)i"
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

rulePounds :: Rule
rulePounds = Rule
  { name = "£"
  , pattern =
    [ regex "lire?"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly Pound
  }

ruleOtherPounds :: Rule
ruleOtherPounds = Rule
  { name = "other pounds"
  , pattern =
    [ regex "lir(a|ă) (egiptian|libanez)(a|ă)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (_:match:_)):_) -> case Text.toLower match of
        "egiptian" -> Just . Token AmountOfMoney $ currencyOnly EGP
        "libanez"  -> Just . Token AmountOfMoney $ currencyOnly LBP
        _          -> Nothing
      _ -> Nothing
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

ruleInr :: Rule
ruleInr = Rule
  { name = "INR"
  , pattern =
    [ regex "rupii?"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly INR
  }

ruleKwd :: Rule
ruleKwd = Rule
  { name = "KWD"
  , pattern =
    [ regex "dinar kuweitian"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly KWD
  }

ruleAed :: Rule
ruleAed = Rule
  { name = "AED"
  , pattern =
    [ regex "dirhami?"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly AED
  }

ruleIntervalBetweenNumeral :: Rule
ruleIntervalBetweenNumeral = Rule
  { name = "between|from <numeral> to|and <amount-of-money>"
  , pattern =
    [ regex "intre|de la"
    , Predicate isPositive
    , regex "[sș]i|la"
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

ruleIntervalBetween :: Rule
ruleIntervalBetween = Rule
  { name = "between|from <amount-of-money> to|and <amount-of-money>"
  , pattern =
    [ regex "intre|de la"
    , Predicate isSimpleAmountOfMoney
    , regex "[sș]i|la"
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
       _) | from < to ->
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

ruleIntervalMax :: Rule
ruleIntervalMax = Rule
  { name = "under/less/lower/no more than <amount-of-money>"
  , pattern =
    [ regex "sub|mai (pu[tț]|ieft)in de|nu chiar|nici macar|cel mult"
    , Predicate isSimpleAmountOfMoney
    ]
  , prod = \tokens -> case tokens of
      (_:
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just to,
                  TAmountOfMoney.currency = c}:
       _) -> Just . Token AmountOfMoney . withMax to $ currencyOnly c
      _ -> Nothing
  }

ruleIntervalMin :: Rule
ruleIntervalMin = Rule
  { name = "over/above/at least/more than <amount-of-money>"
  , pattern =
    [ regex "peste|mai (mult|scump) de|cel pu[tț]in"
    , Predicate isSimpleAmountOfMoney
    ]
  , prod = \tokens -> case tokens of
      (_:
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just to,
                  TAmountOfMoney.currency = c}:
       _) -> Just . Token AmountOfMoney . withMin to $ currencyOnly c
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleUnitAmount
  , ruleAmountDeUnit
  , ruleAed
  , ruleCent
  , ruleDollar
  , ruleInr
  , ruleIntersect
  , ruleIntersectAndNumeral
  , ruleIntersectAndXCents
  , ruleIntersectXCents
  , ruleKwd
  , ruleOtherPounds
  , rulePounds
  , rulePrecisionAmountOfMoney
  , ruleRiyals
  , ruleRon
  , ruleIntervalBetween
  , ruleIntervalBetweenNumeral
  , ruleIntervalDash
  , ruleIntervalMax
  , ruleIntervalMin
  , ruleIntervalNumeralDash
  ]
