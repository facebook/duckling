-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.KO.Rules
  ( rules ) where

import Data.Maybe
import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.AmountOfMoney.Helpers
import Duckling.AmountOfMoney.Types (Currency(..), AmountOfMoneyData (..))
import qualified Duckling.AmountOfMoney.Types as TAmountOfMoney
import Duckling.Types

ruleAmountofmoneyAbout :: Rule
ruleAmountofmoneyAbout = Rule
  { name = "<amount-of-money> about"
  , pattern =
    [ financeWith TAmountOfMoney.value isJust
    , regex "\xc815\xb3c4|\xcbe4"
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> Just token
      _ -> Nothing
  }

ruleAud :: Rule
ruleAud = Rule
  { name = "AUD"
  , pattern =
    [ regex "\xd638\xc8fc\xb2ec\xb7ec"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly AUD
  }

ruleKrw :: Rule
ruleKrw = Rule
  { name = "₩"
  , pattern =
    [ regex "\x20a9|\xc6d0"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly KRW
  }

ruleAboutAmountofmoney :: Rule
ruleAboutAmountofmoney = Rule
  { name = "about <amount-of-money>"
  , pattern =
    [ regex "\xc57d|\xb300\xcda9|\xc5bc\xcd94"
    , financeWith TAmountOfMoney.value isJust
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> Just token
      _ -> Nothing
  }

ruleCent :: Rule
ruleCent = Rule
  { name = "cent"
  , pattern =
    [ regex "cents?|\xc13c(\xd2b8|\xce20)"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly Cent
  }

ruleExactlyAmountofmoney :: Rule
ruleExactlyAmountofmoney = Rule
  { name = "exactly <amount-of-money>"
  , pattern =
    [ regex "\xb531|\xc815\xd655\xd788"
    , financeWith TAmountOfMoney.value isJust
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> Just token
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

ruleEuro :: Rule
ruleEuro = Rule
  { name = "€"
  , pattern =
    [ regex "\x20ac|\xc720\xb85c"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly EUR
  }

ruleDollar :: Rule
ruleDollar = Rule
  { name = "$"
  , pattern =
    [ regex "\xb2ec\xb7ec|\xbd88"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly Dollar
  }

ruleInr :: Rule
ruleInr = Rule
  { name = "INR"
  , pattern =
    [ regex "\xb8e8\xd53c|\xc778\xb3c4\xb8e8\xd53c"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly INR
  }

rulePounds :: Rule
rulePounds = Rule
  { name = "£"
  , pattern =
    [ regex "\xd30c\xc6b4\xb4dc|\xc601\xad6d\xd30c\xc6b4\xb4dc"
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
  [ ruleAboutAmountofmoney
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
