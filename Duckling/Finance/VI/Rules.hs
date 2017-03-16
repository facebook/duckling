-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Finance.VI.Rules
  ( rules ) where

import Data.Maybe
import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Finance.Helpers
import Duckling.Finance.Types (Currency(..), FinanceData (..))
import qualified Duckling.Finance.Types as TFinance
import Duckling.Numeral.Types (NumeralData (..))
import qualified Duckling.Numeral.Types as TNumeral
import Duckling.Types

ruleNg :: Rule
ruleNg = Rule
  { name = "đồng"
  , pattern =
    [ regex "\x0111\x1ed3ng?"
    ]
  , prod = \_ -> Just . Token Finance $ currencyOnly VND
  }

ruleDollar :: Rule
ruleDollar = Rule
  { name = "$"
  , pattern =
    [ regex "\x0111\x00f4 la|\x0111\x00f4 m\x1ef9|\x0111(\x00f4)?"
    ]
  , prod = \_ -> Just . Token Finance $ currencyOnly Dollar
  }

ruleVnd :: Rule
ruleVnd = Rule
  { name = "VNĐ"
  , pattern =
    [ regex "vn(\x0110|\\$)"
    ]
  , prod = \_ -> Just . Token Finance $ currencyOnly VND
  }

ruleCent :: Rule
ruleCent = Rule
  { name = "cent"
  , pattern =
    [ regex "xen|xu?|penn(y|ies)"
    ]
  , prod = \_ -> Just . Token Finance $ currencyOnly Cent
  }

rulePounds :: Rule
rulePounds = Rule
  { name = "£"
  , pattern =
    [ regex "pounds?"
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
       Token Numeral (NumeralData {TNumeral.value = c}):
       _) -> Just . Token Finance $ withCents c fd
      _ -> Nothing
  }

ruleIntersectAndNumeral :: Rule
ruleIntersectAndNumeral = Rule
  { name = "intersect and number"
  , pattern =
    [ financeWith TFinance.value isJust
    , regex "v\x00e0"
    , dimension Numeral
    ]
  , prod = \tokens -> case tokens of
      (Token Finance fd:
       _:
       Token Numeral (NumeralData {TNumeral.value = c}):
       _) -> Just . Token Finance $ withCents c fd
      _ -> Nothing
  }

ruleIntersectXXuxen :: Rule
ruleIntersectXXuxen = Rule
  { name = "intersect (X xu|xen)"
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

ruleIntersectVXXuxen :: Rule
ruleIntersectVXXuxen = Rule
  { name = "intersect (và X xu|xen)"
  , pattern =
    [ financeWith TFinance.value isJust
    , regex "v\x00e0"
    , financeWith TFinance.currency (== Cent)
    ]
  , prod = \tokens -> case tokens of
      (Token Finance fd:
       _:
       Token Finance (FinanceData {TFinance.value = Just c}):
       _) -> Just . Token Finance $ withCents c fd
      _ -> Nothing
  }

ruleDirham :: Rule
ruleDirham = Rule
  { name = "AED"
  , pattern =
    [ regex "AED\\.|dirhams?"
    ]
  , prod = \_ -> Just . Token Finance $ currencyOnly AED
  }

rules :: [Rule]
rules =
  [ ruleCent
  , ruleDirham
  , ruleDollar
  , ruleIntersect
  , ruleIntersectAndNumeral
  , ruleIntersectVXXuxen
  , ruleIntersectXXuxen
  , ruleNg
  , rulePounds
  , ruleVnd
  ]
