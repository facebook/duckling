-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Finance.ID.Rules
  ( rules ) where

import Data.Maybe
import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Finance.Helpers
import Duckling.Finance.Types (Currency(..))
import qualified Duckling.Finance.Types as TFinance
import Duckling.Number.Types (NumberData (..))
import qualified Duckling.Number.Types as TNumber
import Duckling.Types

ruleDollar :: Rule
ruleDollar = Rule
  { name = "$"
  , pattern =
    [ regex "dolar?"
    ]
  , prod = \_ -> Just . Token Finance $ currencyOnly Dollar
  }

ruleIdr :: Rule
ruleIdr = Rule
  { name = "IDR"
  , pattern =
    [ regex "rp\\.?|rupiah"
    ]
  , prod = \_ -> Just . Token Finance $ currencyOnly IDR
  }

rulePounds :: Rule
rulePounds = Rule
  { name = "£"
  , pattern =
    [ regex "pound( sterling)?"
    ]
  , prod = \_ -> Just . Token Finance $ currencyOnly Pound
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

ruleJpy :: Rule
ruleJpy = Rule
  { name = "JPY"
  , pattern =
    [ regex "\x00a5\\."
    ]
  , prod = \_ -> Just . Token Finance $ currencyOnly JPY
  }

rules :: [Rule]
rules =
  [ ruleDollar
  , ruleIdr
  , ruleIntersect
  , ruleJpy
  , rulePounds
  ]
