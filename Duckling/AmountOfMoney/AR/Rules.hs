-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.AR.Rules
  ( rules
  ) where

import Data.Maybe
import Data.String
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

rulePounds :: Rule
rulePounds = Rule
  { name = "£"
  , pattern =
    [ regex "جنيه(ات)?"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly Pound
  }

ruleDinars :: Rule
ruleDinars = Rule
  { name = "dinars"
  , pattern =
    [ regex "دينار|دنانير"
    ]
    , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly Dinar
  }

ruleDirhams :: Rule
ruleDirhams = Rule
  { name = "Dirhams"
  , pattern =
    [ regex "درا?هم"
    ]
    , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly Dirham
  }

ruleRiyals :: Rule
ruleRiyals = Rule
  { name = "Riyals"
  , pattern =
    [ regex "ريال(ات)?"
    ]
    , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly Riyal
  }

ruleDollars :: Rule
ruleDollars = Rule
  { name = "Dollars"
  , pattern =
    [ regex "دولار(ات)?"
    ]
    , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly Dollar
  }

ruleShekel :: Rule
ruleShekel = Rule
  { name = "Shekel"
  , pattern =
    [ regex "شي[كق]ل|شوا[كق]ل"
    ]
    , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly ILS
  }

ruleEuro :: Rule
ruleEuro = Rule
  { name = "Euro"
  , pattern =
    [ regex "[أاي]ورو"
    ]
    , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly EUR
  }

ruleOtherPounds :: Rule
ruleOtherPounds = Rule
  { name = "other pounds"
  , pattern =
    [ regex "جنيه(ات)? (مصر|استرلين)ي?[ةه]?"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (_:match:_)):
       _) -> case Text.toLower match of
        "مصر"     -> Just . Token AmountOfMoney $ currencyOnly EGP
        "استرلين" -> Just . Token AmountOfMoney $ currencyOnly GBP
        _         -> Nothing
      _ -> Nothing
  }

ruleOtherDinars :: Rule
ruleOtherDinars = Rule
  { name = "dinars"
  , pattern =
    [ regex "(دينار|دنانير) ([أا]ردن|كويت|عراق)ي?[ةه]?"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (_:match:_)):
       _) -> case Text.toLower match of
        "كويت" -> Just . Token AmountOfMoney $ currencyOnly KWD
        "اردن" -> Just . Token AmountOfMoney $ currencyOnly JOD
        "أردن" -> Just . Token AmountOfMoney $ currencyOnly JOD
        "عراق" -> Just . Token AmountOfMoney $ currencyOnly IQD
        _      -> Nothing
      _ -> Nothing
  }

ruleOtherDirhams :: Rule
ruleOtherDirhams = Rule
  { name = "dirham"
  , pattern =
    [ regex "(درهم|دراهم) (اردن|أردن|مغرب)ي?[ةه]?"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (_:match:_)):
       _) -> case Text.toLower match of
        "إمارات" -> Just . Token AmountOfMoney $ currencyOnly AED
        "امارات" -> Just . Token AmountOfMoney $ currencyOnly AED
        "مغرب"   -> Just . Token AmountOfMoney $ currencyOnly MAD
        _        -> Nothing
      _ -> Nothing
  }

ruleOtherRiyals :: Rule
ruleOtherRiyals = Rule
  { name = "riyals"
  , pattern =
    [ regex "(ريال|ريالات) (سعود|قطر)ي?[ةه]?"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (_:match:_)):
       _) -> case Text.toLower match of
        "قطر"  -> Just . Token AmountOfMoney $ currencyOnly QAR
        "سعود" -> Just . Token AmountOfMoney $ currencyOnly SAR
        _      -> Nothing
      _ -> Nothing
  }

ruleCent :: Rule
ruleCent = Rule
  { name = "cent"
  , pattern =
    [ regex "سي?نت(ات)?"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly Cent
  }

ruleLBP :: Rule
ruleLBP = Rule
  { name = "LBP"
  , pattern =
    [ regex "لير([ةه]|ات) لبناني[ةه]?"
    ]
    , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly LBP
  }

ruleIntersectAndXCents :: Rule
ruleIntersectAndXCents = Rule
  { name = "intersect (and X cents)"
  , pattern =
    [ Predicate isWithoutCents
    , regex "و"
    , Predicate isCents
    ]
  , prod = \tokens -> case tokens of
      (Token AmountOfMoney fd:
       _:
       Token AmountOfMoney AmountOfMoneyData {TAmountOfMoney.value = Just c}:
       _) -> Just . Token AmountOfMoney $ withCents c fd
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
       Token Numeral NumeralData {TNumeral.value = c}:
       _) -> Just . Token AmountOfMoney $ withCents c fd
      _ -> Nothing
  }

ruleIntersectAndNumeral :: Rule
ruleIntersectAndNumeral = Rule
  { name = "intersect (and number)"
  , pattern =
    [ Predicate isWithoutCents
    , regex "و"
    , Predicate isNatural
    ]
  , prod = \tokens -> case tokens of
      (Token AmountOfMoney fd:
       _:
       Token Numeral NumeralData {TNumeral.value = c}:
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
       Token AmountOfMoney AmountOfMoneyData {TAmountOfMoney.value = Just c}:
       _) -> Just . Token AmountOfMoney $ withCents c fd
      _ -> Nothing
  }

rulePrecision :: Rule
rulePrecision = Rule
  { name = "about|exactly <amount-of-money>"
  , pattern =
    [ regex "حوال[ي|ى]|تقريبا|بحدود"
    , Predicate isMoneyWithValue
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> Just token
      _ -> Nothing
  }

ruleIntervalBetweenNumeral :: Rule
ruleIntervalBetweenNumeral = Rule
  { name = "between|from <numeral> to|and <amount-of-money>"
  , pattern =
    [ regex "(من|(ما )?بين)( ال)?"
    , Predicate isPositive
    , regex "(الى|حتى|و|ل)( ا?ل)?"
    , Predicate isSimpleAmountOfMoney
    ]
  , prod = \tokens -> case tokens of
      (_:
       Token Numeral NumeralData {TNumeral.value = from}:
       _:
       Token AmountOfMoney AmountOfMoneyData {TAmountOfMoney.value = Just to, TAmountOfMoney.currency = c}:
       _) | from < to ->
        Just . Token AmountOfMoney . withInterval (from, to) $ currencyOnly c
      _ -> Nothing
  }

ruleIntervalBetween :: Rule
ruleIntervalBetween = Rule
  { name = "between|from <amount-of-money> to|and <amount-of-money>"
  , pattern =
    [ regex "(من|(ما )?بين)( ال)?"
    , Predicate isSimpleAmountOfMoney
    , regex "(الى|حتى|و|ل)( ا?ل)?"
    , Predicate isSimpleAmountOfMoney
    ]
  , prod = \tokens -> case tokens of
      (_:
       Token AmountOfMoney AmountOfMoneyData {TAmountOfMoney.value = Just from, TAmountOfMoney.currency = c1}:
       _:
       Token AmountOfMoney AmountOfMoneyData {TAmountOfMoney.value = Just to, TAmountOfMoney.currency = c2}:
       _) | from < to && c1 == c2 ->
        Just . Token AmountOfMoney . withInterval (from, to) $ currencyOnly c1
      _ -> Nothing
  }

ruleIntervalNumeralDash :: Rule
ruleIntervalNumeralDash = Rule
  { name = "<numeral> - <amount-of-money>"
  , pattern =
    [ Predicate isPositive
    , regex "-"
    , Predicate isSimpleAmountOfMoney
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData {TNumeral.value = from}:
       _:
       Token AmountOfMoney AmountOfMoneyData {TAmountOfMoney.value = Just to, TAmountOfMoney.currency = c}:
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
      (Token AmountOfMoney AmountOfMoneyData {TAmountOfMoney.value = Just from, TAmountOfMoney.currency = c1}:
       _:
       Token AmountOfMoney AmountOfMoneyData {TAmountOfMoney.value = Just to, TAmountOfMoney.currency = c2}:
       _) | from < to && c1 == c2 ->
        Just . Token AmountOfMoney . withInterval (from, to) $ currencyOnly c1
      _ -> Nothing
  }

ruleIntervalMax :: Rule
ruleIntervalMax = Rule
  { name = "under/less/lower/no more than <amount-of-money>"
  , pattern =
    [ regex "(حتى|[أا]قل من|تحت|(ما )?دون)|على ال[أا]كثر"
    , Predicate isSimpleAmountOfMoney
    ]
  , prod = \tokens -> case tokens of
      (_:
       Token AmountOfMoney AmountOfMoneyData {TAmountOfMoney.value = Just to, TAmountOfMoney.currency = c}:
       _) -> Just . Token AmountOfMoney . withMax to $ currencyOnly c
      _ -> Nothing
  }

ruleIntervalAtMost :: Rule
ruleIntervalAtMost = Rule
  { name = "under/less/lower/no more than <amount-of-money>"
  , pattern =
    [ regex "على ال[أا]كثر"
    , Predicate isSimpleAmountOfMoney
    ]
  , prod = \tokens -> case tokens of
      (_:
       Token AmountOfMoney AmountOfMoneyData {TAmountOfMoney.value = Just to, TAmountOfMoney.currency = c}:
       _) -> Just . Token AmountOfMoney . withMax to $ currencyOnly c
      _ -> Nothing
  }

ruleIntervalMin :: Rule
ruleIntervalMin = Rule
  { name = "at least/over/above/more than <amount-of-money>"
  , pattern =
    [ regex "فوق||[أا]كثر من|على ال[اأ]قل"
    , Predicate isSimpleAmountOfMoney
    ]
  , prod = \tokens -> case tokens of
      (_:
       Token AmountOfMoney AmountOfMoneyData {TAmountOfMoney.value = Just to, TAmountOfMoney.currency = c}:
       _) -> Just . Token AmountOfMoney . withMin to $ currencyOnly c
      _ -> Nothing
  }

ruleIntervalAtLeast :: Rule
ruleIntervalAtLeast = Rule
  { name = "<amount-of-money> at least"
  , pattern =
    [ Predicate isSimpleAmountOfMoney
    , regex "على ال[اأ]قل"
    ]
  , prod = \tokens -> case tokens of
      (Token AmountOfMoney AmountOfMoneyData {TAmountOfMoney.value = Just to, TAmountOfMoney.currency = c}:
       _) -> Just . Token AmountOfMoney . withMin to $ currencyOnly c
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleUnitAmount
  , ruleCent
  , rulePounds
  , ruleDinars
  , ruleDirhams
  , ruleRiyals
  , ruleDollars
  , ruleShekel
  , ruleEuro
  , ruleOtherPounds
  , ruleOtherDinars
  , ruleOtherDirhams
  , ruleOtherRiyals
  , ruleLBP
  , ruleIntersect
  , ruleIntersectAndNumeral
  , ruleIntersectAndXCents
  , ruleIntersectXCents
  , ruleIntervalBetweenNumeral
  , ruleIntervalBetween
  , ruleIntervalMax
  , ruleIntervalAtMost
  , ruleIntervalMin
  , ruleIntervalAtLeast
  , ruleIntervalNumeralDash
  , ruleIntervalDash
  , rulePrecision
  ]
