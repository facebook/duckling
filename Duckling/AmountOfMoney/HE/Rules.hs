-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Duckling.AmountOfMoney.HE.Rules
  ( rules
  ) where

import Data.Maybe
import Data.String
import Prelude

import Duckling.AmountOfMoney.Helpers
import Duckling.AmountOfMoney.Types
  ( Currency(..)
  , AmountOfMoneyData(..)
  )
import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers
  ( isPositive
  )
import Duckling.Numeral.Types (NumeralData(..))
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

ruleOneShekel :: Rule
ruleOneShekel = Rule
  { name    = "שקל"
  , pattern = [regex "שקל( אחד| בודד)?"]
  , prod    = \_ -> Just . Token AmountOfMoney $ withValue 1 $ currencyOnly ILS
  }
ruleTwoShekel :: Rule
ruleTwoShekel = Rule
  { name    = "שנקל"
  , pattern = [regex "שנקל"]
  , prod    = \_ -> Just . Token AmountOfMoney $ withValue 2 $ currencyOnly ILS
  }
ruleOneAgura :: Rule
ruleOneAgura = Rule
  { name    = "oneAgura"
  , pattern = [regex "אגורה( אחת| בודדת)?"]
  , prod    = \_ -> Just . Token AmountOfMoney $ withValue 0.01 $
                    currencyOnly ILS
  }
ruleShekel :: Rule
ruleShekel = Rule
  { name    = "שקל"
  , pattern = [regex "שקל(ים)?|ש״?ח"]
  , prod    = \_ -> Just . Token AmountOfMoney $ currencyOnly ILS
  }

ruleThousandShekel :: Rule
ruleThousandShekel = Rule
    { name = "אש״ח"
    , pattern =
      [ Predicate isPositive
      , regex "אש״?ח"
      ]
    , prod = \case
        (Token Numeral NumeralData{TNumeral.value = v}:
         _) -> Just . Token AmountOfMoney $ withValue (v*1000) $
               currencyOnly ILS
        _ -> Nothing
    }

ruleMillionShekel :: Rule
ruleMillionShekel = Rule
    { name = "מש״ח"
    , pattern =
      [ Predicate isPositive
      , regex "מש״?ח"
      ]
    , prod = \case
        (Token Numeral NumeralData{TNumeral.value = v}:
         _) -> Just . Token AmountOfMoney $ withValue (v*1000000) $
               currencyOnly ILS
        _ -> Nothing
    }
ruleAgura :: Rule
ruleAgura = Rule
    { name = "multiple agura"
    , pattern =
      [ Predicate isPositive
      , regex "אגורות"
      ]
    , prod = \case
        (Token Numeral NumeralData{TNumeral.value = v}:
         _) -> Just . Token AmountOfMoney $ withValue (v*0.01) $
               currencyOnly ILS
        _ -> Nothing
    }

ruleIntersect :: Rule
ruleIntersect = Rule
  { name = "intersect"
  , pattern =
    [ Predicate isWithoutCents
    , regex "ו"
    , Predicate isPositive
    ]
  , prod = \case
      (Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just v,
                  TAmountOfMoney.currency = c}:
       _:
       Token Numeral NumeralData { TNumeral.value = p }:
       _) -> Just . Token AmountOfMoney $ withValue (v + p) $ currencyOnly c
      _ -> Nothing
  }

ruleIntersectAndXCents :: Rule
ruleIntersectAndXCents = Rule
  { name = "intersect (and X cents)"
  , pattern =
    [ Predicate isWithoutCents
    , regex "ו"
    , Predicate isCents
    ]
  , prod = \case
      (Token AmountOfMoney fd :
       _ :
       Token AmountOfMoney AmountOfMoneyData { TAmountOfMoney.value = Just c }:
       _) -> Just . Token AmountOfMoney $ withCents c fd
      _ -> Nothing
  }

rulePounds :: Rule
rulePounds = Rule
  { name = "£"
  , pattern =
    [ regex "פאונד(ים)?"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly Pound
  }

ruleDollar :: Rule
ruleDollar = Rule
  { name    = "$"
  , pattern =
    [ regex "דולר(ים)?"
    ]
  , prod    = \_ -> Just . Token AmountOfMoney $ currencyOnly Dollar
  }

ruleCent :: Rule
ruleCent = Rule
  { name = "cent"
  , pattern =
      [ regex "סנט(ים)?"
      ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly Cent
  }

ruleEUR :: Rule
ruleEUR = Rule
  { name    = "€"
  , pattern =
    [ regex "אירו|יורו"
    ]
  , prod    = \_ -> Just . Token AmountOfMoney $ currencyOnly EUR
  }
ruleOneGBP :: Rule
ruleOneGBP = Rule
  { name = "OneGBP"
  , pattern =
    [ regex "לירה שטרלינג"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ withValue 1 $ currencyOnly GBP
  }
ruleGBP :: Rule
ruleGBP = Rule
  { name = "OneGBP"
  , pattern =
    [ regex "לירות שטרלינג"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly GBP
  }

rulePrecision :: Rule
rulePrecision = Rule
  { name = "about|exactly <amount-of-money>"
  , pattern =
      [ regex "בערך|בדיוק|קרוב ל"
      , Predicate isMoneyWithValue
      ]
  , prod = \case
      (_:
       token:
       _) -> Just token
      _ -> Nothing
  }

ruleIntervalBetweenNumeral :: Rule
ruleIntervalBetweenNumeral = Rule
  { name = "between|from <numeral> to|and <amount-of-money>"
  , pattern =
      [ regex "מ?|בין "
      , Predicate isPositive
      , regex "עד |ל"
      , Predicate isSimpleAmountOfMoney
      ]
  , prod = \case
      (_:
       Token Numeral NumeralData { TNumeral.value = from }:
       _:
       Token AmountOfMoney AmountOfMoneyData { TAmountOfMoney.value = Just to,
                  TAmountOfMoney.currency = c }:
       _) | from < to ->
        Just . Token AmountOfMoney . withInterval (from, to) $ currencyOnly c
      _ -> Nothing
  }

ruleIntervalBetween :: Rule
ruleIntervalBetween = Rule
  { name = "between|from <amount-of-money> to|and <amount-of-money>"
  , pattern =
      [ regex "מ?|בין "
      , Predicate isSimpleAmountOfMoney
      , regex "עד |ל"
      , Predicate isSimpleAmountOfMoney
      ]
  , prod = \case
      (_:
       Token AmountOfMoney AmountOfMoneyData { TAmountOfMoney.value = Just from,
                  TAmountOfMoney.currency = c1 }:
       _:
       Token AmountOfMoney AmountOfMoneyData { TAmountOfMoney.value = Just to,
                  TAmountOfMoney.currency = c2 }:
       _) | from < to && c1 == c2 ->
            Just . Token AmountOfMoney . withInterval (from, to) $
              currencyOnly c1
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
  , prod = \case
      (Token Numeral NumeralData { TNumeral.value = from }:
       _:
       Token AmountOfMoney AmountOfMoneyData { TAmountOfMoney.value = Just to,
                  TAmountOfMoney.currency = c }:
       _) | from < to -> Just . Token AmountOfMoney . withInterval (from, to) $
            currencyOnly c
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
      (Token AmountOfMoney AmountOfMoneyData { TAmountOfMoney.value = Just from,
                  TAmountOfMoney.currency = c1 }:
       _:
       Token AmountOfMoney AmountOfMoneyData { TAmountOfMoney.value = Just to,
                  TAmountOfMoney.currency = c2 }:
       _) | from < to && c1 == c2 ->
            Just . Token AmountOfMoney . withInterval (from, to) $
              currencyOnly c1
      _ -> Nothing
  }

ruleIntervalMax :: Rule
ruleIntervalMax = Rule
  { name = "under/less/lower/no more than <amount-of-money>"
  , pattern =
      [ regex "פחות מ|עד|לא יותר מ|מתחת ל?|לא מעל"
      , Predicate isSimpleAmountOfMoney
      ]
  , prod = \case
      (_:
       Token AmountOfMoney AmountOfMoneyData { TAmountOfMoney.value = Just to,
                  TAmountOfMoney.currency = c }:
       _) -> Just . Token AmountOfMoney . withMax to $ currencyOnly c
      _ -> Nothing
  }

ruleIntervalMin :: Rule
ruleIntervalMin = Rule
  { name = "over/above/at least/more than <amount-of-money>"
  , pattern =
      [ regex "יותר מ|מעל|מ|לא פחות מ|לא מתחת ל"
      , Predicate isSimpleAmountOfMoney
      ]
  , prod = \case
      (_:
       Token AmountOfMoney AmountOfMoneyData { TAmountOfMoney.value = Just to,
                  TAmountOfMoney.currency = c }:
       _) -> Just . Token AmountOfMoney . withMin to $ currencyOnly c
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleUnitAmount
  , ruleCent
  , ruleEUR
  , ruleIntersectAndXCents
  , ruleIntervalBetweenNumeral
  , ruleIntervalBetween
  , ruleIntervalMax
  , ruleIntervalMin
  , ruleIntervalNumeralDash
  , ruleIntervalDash
  , rulePounds
  , ruleOneGBP
  , ruleGBP
  , rulePrecision
  , ruleIntersect
  , ruleAgura
  , ruleShekel
  , ruleOneShekel
  , ruleTwoShekel
  , ruleOneAgura
  , ruleThousandShekel
  , ruleMillionShekel
  , ruleDollar
  ]
