-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.TR.Rules
  ( rules
  ) where

import Data.String (fromString)
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

rulePounds :: Rule
rulePounds = Rule
  { name = "₺"
  , pattern =
    [ regex "₺"
    ]
  , prod = \_ -> Just $ Token AmountOfMoney $ currencyOnly TRY
  }

ruleCent :: Rule
ruleCent = Rule
  { name = "cent"
  , pattern =
    [ regex "kuruş?"
    ]
  , prod = \_ -> Just $ Token AmountOfMoney $ currencyOnly Cent
  }

ruleADollarCoin :: Rule
ruleADollarCoin = Rule
  { name = "a <dollar coin>"
  , pattern =
    [ regex "kuruş"
    , Predicate isDollarCoin
    ]
  , prod = \tokens -> case tokens of
      (_:
       Token AmountOfMoney fd:
       _) -> Just $ Token AmountOfMoney fd
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
       _) -> Just $ Token AmountOfMoney $ withValue (c * d) $ currencyOnly cur
      _ -> Nothing
  }

ruleIntersectAndXCents :: Rule
ruleIntersectAndXCents = Rule
  { name = "intersect (and X cents)"
  , pattern =
    [ Predicate isWithoutCents
    , regex "(lira|tl)"
    , Predicate isCents
    ]
  , prod = \tokens -> case tokens of
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
  , prod = \tokens -> case tokens of
      (Token AmountOfMoney fd:
       Token Numeral NumeralData{TNumeral.value = c}:
       _) -> Just $ Token AmountOfMoney $ withCents c fd
      _ -> Nothing
  }

ruleIntersectAndNumeral :: Rule
ruleIntersectAndNumeral = Rule
  { name = "intersect (and number)"
  , pattern =
    [ Predicate isWithoutCents
    , regex "(lira|tl)"
    , Predicate isNatural
    ]
  , prod = \tokens -> case tokens of
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
  , prod = \tokens -> case tokens of
      (Token AmountOfMoney fd:
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just c}:
       _) -> Just $ Token AmountOfMoney $ withCents c fd
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleUnitAmount
  , ruleCent
  , ruleADollarCoin
  , ruleNumDollarCoins
  , ruleIntersect
  , ruleIntersectAndNumeral
  , ruleIntersectAndXCents
  , ruleIntersectXCents
  , rulePounds
  ]
