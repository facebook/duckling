-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.EN.Rules
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
    [ regex "pounds?"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly Pound
  }

ruleOtherPounds :: Rule
ruleOtherPounds = Rule
  { name = "other pounds"
  , pattern =
    [ regex "(egyptian|lebanese) ?pounds?"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "egyptian" -> Just . Token AmountOfMoney $ currencyOnly EGP
        "lebanese" -> Just . Token AmountOfMoney $ currencyOnly LBP
        _          -> Nothing
      _ -> Nothing
  }

ruleRiyals :: Rule
ruleRiyals = Rule
  { name = "riyals"
  , pattern =
    [ regex "(qatari|saudi) ?riyals?"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "qatari" -> Just . Token AmountOfMoney $ currencyOnly QAR
        "saudi"  -> Just . Token AmountOfMoney $ currencyOnly SAR
        _        -> Nothing
      _ -> Nothing
  }

ruleDinars :: Rule
ruleDinars = Rule
  { name = "dinars"
  , pattern =
    [ regex "(kuwaiti) ?dinars?"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "kuwaiti" -> Just . Token AmountOfMoney $ currencyOnly KWD
        _         -> Nothing
      _ -> Nothing
  }

ruleDirham :: Rule
ruleDirham = Rule
  { name = "dirham"
  , pattern =
    [ regex "dirhams?"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly AED
  }

ruleRinggit :: Rule
ruleRinggit = Rule
  { name = "ringgit"
  , pattern =
    [ regex "(malaysian? )?ringgits?"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly MYR
  }

ruleCent :: Rule
ruleCent = Rule
  { name = "cent"
  , pattern =
    [ regex "cents?|penn(y|ies)|pence|sens?"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly Cent
  }

ruleBucks :: Rule
ruleBucks = Rule
  { name = "bucks"
  , pattern =
    [ regex "bucks?"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly Unnamed
  }

ruleACurrency :: Rule
ruleACurrency = Rule
  { name = "a <currency>"
  , pattern =
    [ regex "an?"
    , Predicate isCurrencyOnly
    ]
  , prod = \tokens -> case tokens of
      (_:
       Token AmountOfMoney fd:
       _) -> Just . Token AmountOfMoney $ fd {TAmountOfMoney.value = Just 1}
      _ -> Nothing
  }

ruleAbsorbA :: Rule
ruleAbsorbA = Rule
  { name = "a <amount-of-money>"
  , pattern =
    [ regex "an?"
    , Predicate isSimpleAmountOfMoney
    ]
  , prod = \case
      (_:Token AmountOfMoney fd:_) -> Just $ Token AmountOfMoney fd
      _ -> Nothing
  }

ruleADollarCoin :: Rule
ruleADollarCoin = Rule
  { name = "a <dollar coin>"
  , pattern =
    [ regex "an?"
    , Predicate isDollarCoin
    ]
  , prod = \tokens -> case tokens of
      (_:
       Token AmountOfMoney fd:
       _) -> Just . Token AmountOfMoney $ fd
      _ -> Nothing
  }

ruleNumDollarCoins :: Rule
ruleNumDollarCoins = Rule
  { name = "X <dollar coins>"
  , pattern =
    [ Predicate isNatural
    , Predicate isDollarCoin
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = c}:
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just d,
                                             TAmountOfMoney.currency = cur}:
       _) -> Just . Token AmountOfMoney $ withValue (c * d) $ currencyOnly cur
      _ -> Nothing
  }

ruleIntersectAndXCents :: Rule
ruleIntersectAndXCents = Rule
  { name = "intersect (and X cents)"
  , pattern =
    [ Predicate isWithoutCents
    , regex "and"
    , Predicate isCents
    ]
  , prod = \tokens -> case tokens of
      (Token AmountOfMoney fd:
       _:
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just c}:
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
       Token Numeral NumeralData{TNumeral.value = c}:
       _) -> Just . Token AmountOfMoney $ withCents c fd
      _ -> Nothing
  }

ruleIntersectAndNumeral :: Rule
ruleIntersectAndNumeral = Rule
  { name = "intersect (and number)"
  , pattern =
    [ Predicate isWithoutCents
    , regex "and"
    , Predicate isNatural
    ]
  , prod = \tokens -> case tokens of
      (Token AmountOfMoney fd:
       _:
       Token Numeral NumeralData{TNumeral.value = c}:
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

rulePrecision :: Rule
rulePrecision = Rule
  { name = "about|exactly <amount-of-money>"
  , pattern =
    [ regex "exactly|precisely|about|approx(\\.|imately)?|close to|near( to)?|around|almost"
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
    [ regex "between|from"
    , Predicate isPositive
    , regex "to|and"
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
    [ regex "between|from"
    , Predicate isSimpleAmountOfMoney
    , regex "to|and"
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

ruleIntervalMax :: Rule
ruleIntervalMax = Rule
  { name = "under/less/lower/no more than <amount-of-money>"
  , pattern =
    [ regex "under|(less|lower|not? more) than"
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
    [ regex "over|above|at least|more than"
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
  , ruleACurrency
  , ruleAbsorbA
  , ruleBucks
  , ruleCent
  , ruleADollarCoin
  , ruleNumDollarCoins
  , ruleDinars
  , ruleDirham
  , ruleIntersect
  , ruleIntersectAndNumeral
  , ruleIntersectAndXCents
  , ruleIntersectXCents
  , ruleIntervalBetweenNumeral
  , ruleIntervalBetween
  , ruleIntervalMax
  , ruleIntervalMin
  , ruleIntervalNumeralDash
  , ruleIntervalDash
  , ruleOtherPounds
  , rulePounds
  , rulePrecision
  , ruleRinggit
  , ruleRiyals
  ]
