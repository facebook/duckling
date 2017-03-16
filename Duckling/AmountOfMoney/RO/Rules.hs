-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.RO.Rules
  ( rules ) where

import Data.Maybe
import qualified Data.Text as Text
import Prelude
import Data.String

import Duckling.AmountOfMoney.Helpers
import Duckling.AmountOfMoney.Types (Currency(..), AmountOfMoneyData (..))
import qualified Duckling.AmountOfMoney.Types as TAmountOfMoney
import Duckling.Dimensions.Types
import Duckling.Numeral.Types (NumeralData (..))
import qualified Duckling.Numeral.Types as TNumeral
import Duckling.Regex.Types
import Duckling.Types

ruleIntersectAndNumeral :: Rule
ruleIntersectAndNumeral = Rule
  { name = "intersect (and number)"
  , pattern =
    [ financeWith TAmountOfMoney.value isJust
    , regex "(s|\x0219)i"
    , dimension Numeral
    ]
  , prod = \tokens -> case tokens of
      (Token AmountOfMoney fd:
       _:
       Token Numeral (NumeralData {TNumeral.value = c}):
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

rulePrecisionAmountofmoney :: Rule
rulePrecisionAmountofmoney = Rule
  { name = "about/exactly <amount-of-money>"
  , pattern =
    [ regex "exact|cam|aprox(\\.|imativ)?|aproape|(i|\x00ee)n jur (de)?"
    , financeWith TAmountOfMoney.value isJust
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> Just token
      _ -> Nothing
  }

ruleCent :: Rule
ruleCent = Rule
  { name = "cent|bani"
  , pattern =
    [ regex "bani?|cen(t|\x021b)i?|c|\x00a2"
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
    [ financeWith TAmountOfMoney.value isJust
    , regex "(s|\x0219)i"
    , financeWith TAmountOfMoney.currency (== Cent)
    ]
  , prod = \tokens -> case tokens of
      (Token AmountOfMoney fd:
       _:
       Token AmountOfMoney (AmountOfMoneyData {TAmountOfMoney.value = Just c}):
       _) -> Just . Token AmountOfMoney $ withCents c fd
      _ -> Nothing
  }

ruleIntersectXCents :: Rule
ruleIntersectXCents = Rule
  { name = "intersect (X cents)"
  , pattern =
    [ financeWith TAmountOfMoney.value isJust
    , financeWith TAmountOfMoney.currency (== Cent)
    ]
  , prod = \tokens -> case tokens of
      (Token AmountOfMoney fd:
       Token AmountOfMoney (AmountOfMoneyData {TAmountOfMoney.value = Just c}):
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
    [ regex "lir(a|\x0103) (egiptian|libanez)(a|\x0103)"
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
    [ financeWith TAmountOfMoney.value isJust
    , dimension Numeral
    ]
  , prod = \tokens -> case tokens of
      (Token AmountOfMoney fd:
       Token Numeral (NumeralData {TNumeral.value = c}):
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

rules :: [Rule]
rules =
  [ ruleAed
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
  , rulePrecisionAmountofmoney
  , ruleRiyals
  , ruleRon
  ]
