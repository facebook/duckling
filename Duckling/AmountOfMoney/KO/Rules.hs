-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


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
import Duckling.Numeral.Helpers (isPositive)
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
    [ regex "cents?|센(트|츠)"
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
    [ regex "dirhams?"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly AED
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
  ]
