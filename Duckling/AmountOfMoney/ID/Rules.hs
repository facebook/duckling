-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.ID.Rules
  ( rules ) where

import Data.Maybe
import Prelude
import Data.String

import Duckling.AmountOfMoney.Helpers
import Duckling.AmountOfMoney.Types (Currency(..))
import qualified Duckling.AmountOfMoney.Types as TAmountOfMoney
import Duckling.Dimensions.Types
import Duckling.Numeral.Types (NumeralData (..))
import qualified Duckling.Numeral.Types as TNumeral
import Duckling.Types

ruleDollar :: Rule
ruleDollar = Rule
  { name = "$"
  , pattern =
    [ regex "dolar?"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly Dollar
  }

ruleIdr :: Rule
ruleIdr = Rule
  { name = "IDR"
  , pattern =
    [ regex "rp\\.?|rupiah"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly IDR
  }

rulePounds :: Rule
rulePounds = Rule
  { name = "£"
  , pattern =
    [ regex "pound( sterling)?"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly Pound
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

ruleJpy :: Rule
ruleJpy = Rule
  { name = "JPY"
  , pattern =
    [ regex "\x00a5\\."
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly JPY
  }

rules :: [Rule]
rules =
  [ ruleDollar
  , ruleIdr
  , ruleIntersect
  , ruleJpy
  , rulePounds
  ]
