-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Finance.RO.Rules
  ( rules ) where

import Data.Maybe
import qualified Data.Text as Text
import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Finance.Helpers
import Duckling.Finance.Types (Currency(..), FinanceData (..))
import qualified Duckling.Finance.Types as TFinance
import Duckling.Number.Types (NumberData (..))
import qualified Duckling.Number.Types as TNumber
import Duckling.Regex.Types
import Duckling.Types

ruleIntersectAndNumber :: Rule
ruleIntersectAndNumber = Rule
  { name = "intersect (and number)"
  , pattern =
    [ financeWith TFinance.value isJust
    , regex "(s|\x0219)i"
    , dimension Numeral
    ]
  , prod = \tokens -> case tokens of
      (Token Finance fd:
       _:
       Token Numeral (NumberData {TNumber.value = c}):
       _) -> Just . Token Finance $ withCents c fd
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
        "saudi"     -> Just . Token Finance $ currencyOnly SAR
        "saudit"    -> Just . Token Finance $ currencyOnly SAR
        "qataria"   -> Just . Token Finance $ currencyOnly QAR
        "qatarian"  -> Just . Token Finance $ currencyOnly QAR
        _           -> Nothing
      _ -> Nothing
  }

ruleDollar :: Rule
ruleDollar = Rule
  { name = "$"
  , pattern =
    [ regex "dolari?"
    ]
  , prod = \_ -> Just . Token Finance $ currencyOnly Dollar
  }

rulePrecisionAmountofmoney :: Rule
rulePrecisionAmountofmoney = Rule
  { name = "about/exactly <amount-of-money>"
  , pattern =
    [ regex "exact|cam|aprox(\\.|imativ)?|aproape|(i|\x00ee)n jur (de)?"
    , financeWith TFinance.value isJust
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
  , prod = \_ -> Just . Token Finance $ currencyOnly Cent
  }

ruleRon :: Rule
ruleRon = Rule
  { name = "RON"
  , pattern =
    [ regex "roni|lei"
    ]
  , prod = \_ -> Just . Token Finance $ currencyOnly RON
  }

ruleIntersectAndXCents :: Rule
ruleIntersectAndXCents = Rule
  { name = "intersect (and X cents)"
  , pattern =
    [ financeWith TFinance.value isJust
    , regex "(s|\x0219)i"
    , financeWith TFinance.currency (== Cent)
    ]
  , prod = \tokens -> case tokens of
      (Token Finance fd:
       _:
       Token Finance (FinanceData {TFinance.value = Just c}):
       _) -> Just . Token Finance $ withCents c fd
      _ -> Nothing
  }

ruleIntersectXCents :: Rule
ruleIntersectXCents = Rule
  { name = "intersect (X cents)"
  , pattern =
    [ financeWith TFinance.value isJust
    , financeWith TFinance.currency (== Cent)
    ]
  , prod = \tokens -> case tokens of
      (Token Finance fd:
       Token Finance (FinanceData {TFinance.value = Just c}):
       _) -> Just . Token Finance $ withCents c fd
      _ -> Nothing
  }

rulePounds :: Rule
rulePounds = Rule
  { name = "£"
  , pattern =
    [ regex "lire?"
    ]
  , prod = \_ -> Just . Token Finance $ currencyOnly Pound
  }

ruleOtherPounds :: Rule
ruleOtherPounds = Rule
  { name = "other pounds"
  , pattern =
    [ regex "lir(a|\x0103) (egiptian|libanez)(a|\x0103)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (_:match:_)):_) -> case Text.toLower match of
        "egiptian" -> Just . Token Finance $ currencyOnly EGP
        "libanez"  -> Just . Token Finance $ currencyOnly LBP
        _          -> Nothing
      _ -> Nothing
  }

ruleIntersect :: Rule
ruleIntersect = Rule
  { name = "intersect"
  , pattern =
    [ financeWith TFinance.value isJust
    , dimension Numeral
    ]
  , prod = \tokens -> case tokens of
      (Token Finance fd:
       Token Numeral (NumberData {TNumber.value = c}):
       _) -> Just . Token Finance $ withCents c fd
      _ -> Nothing
  }

ruleInr :: Rule
ruleInr = Rule
  { name = "INR"
  , pattern =
    [ regex "rupii?"
    ]
  , prod = \_ -> Just . Token Finance $ currencyOnly INR
  }

ruleKwd :: Rule
ruleKwd = Rule
  { name = "KWD"
  , pattern =
    [ regex "dinar kuweitian"
    ]
  , prod = \_ -> Just . Token Finance $ currencyOnly KWD
  }

ruleAed :: Rule
ruleAed = Rule
  { name = "AED"
  , pattern =
    [ regex "dirhami?"
    ]
  , prod = \_ -> Just . Token Finance $ currencyOnly AED
  }

rules :: [Rule]
rules =
  [ ruleAed
  , ruleCent
  , ruleDollar
  , ruleInr
  , ruleIntersect
  , ruleIntersectAndNumber
  , ruleIntersectAndXCents
  , ruleIntersectXCents
  , ruleKwd
  , ruleOtherPounds
  , rulePounds
  , rulePrecisionAmountofmoney
  , ruleRiyals
  , ruleRon
  ]
