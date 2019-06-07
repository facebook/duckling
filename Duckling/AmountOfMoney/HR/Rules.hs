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

rules :: [Rule]
rules =
  [ ruleUnitAmount
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
  ]
