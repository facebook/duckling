-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Finance.EN.Rules
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

rulePounds :: Rule
rulePounds = Rule
  { name = "£"
  , pattern =
    [ regex "pounds?"
    ]
  , prod = \_ -> Just . Token Finance $ currencyOnly Pound
  }

ruleOtherPounds :: Rule
ruleOtherPounds = Rule
  { name = "other pounds"
  , pattern =
    [ regex "(egyptian|lebanese) ?pounds?"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "egyptian" -> Just . Token Finance $ currencyOnly EGP
        "lebanese" -> Just . Token Finance $ currencyOnly LBP
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
        "qatari" -> Just . Token Finance $ currencyOnly QAR
        "saudi"  -> Just . Token Finance $ currencyOnly SAR
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
        "kuwaiti" -> Just . Token Finance $ currencyOnly KWD
        _        -> Nothing
      _ -> Nothing
  }

ruleDirham :: Rule
ruleDirham = Rule
  { name = "dirham"
  , pattern =
    [ regex "dirhams?"
    ]
  , prod = \_ -> Just . Token Finance $ currencyOnly AED
  }

ruleCent :: Rule
ruleCent = Rule
  { name = "cent"
  , pattern =
    [ regex "cents?|penn(y|ies)"
    ]
  , prod = \_ -> Just . Token Finance $ currencyOnly Cent
  }

ruleBucks :: Rule
ruleBucks = Rule
  { name = "bucks"
  , pattern =
    [ regex "bucks?"
    ]
  , prod = \_ -> Just . Token Finance $ currencyOnly Unnamed
  }

ruleIntersectAndXCents :: Rule
ruleIntersectAndXCents = Rule
  { name = "intersect (and X cents)"
  , pattern =
    [ financeWith TFinance.value isJust
    , regex "and"
    , financeWith TFinance.currency (== Cent)
    ]
  , prod = \tokens -> case tokens of
      (Token Finance fd:
       _:
       Token Finance (FinanceData {TFinance.value = Just c}):
       _) -> Just . Token Finance $ withCents c fd
      _ -> Nothing
  }

ruleIntersect :: Rule
ruleIntersect = Rule
  { name = "intersect"
  , pattern =
    [ financeWith TFinance.value isJust
    , dimension DNumber
    ]
  , prod = \tokens -> case tokens of
      (Token Finance fd:
       Token DNumber (NumberData {TNumber.value = c}):
       _) -> Just . Token Finance $ withCents c fd
      _ -> Nothing
  }

ruleIntersectAndNumber :: Rule
ruleIntersectAndNumber = Rule
  { name = "intersect (and number)"
  , pattern =
    [ financeWith TFinance.value isJust
    , regex "and"
    , dimension DNumber
    ]
  , prod = \tokens -> case tokens of
      (Token Finance fd:
       _:
       Token DNumber (NumberData {TNumber.value = c}):
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

rulePrecision :: Rule
rulePrecision = Rule
  { name = "about|exactly <amount-of-money>"
  , pattern =
    [ regex "exactly|precisely|about|approx(\\.|imately)?|close to|near( to)?|around|almost"
    , financeWith TFinance.value isJust
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> Just token
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleBucks
  , ruleCent
  , ruleDinars
  , ruleDirham
  , ruleIntersect
  , ruleIntersectAndNumber
  , ruleIntersectAndXCents
  , ruleIntersectXCents
  , ruleOtherPounds
  , rulePounds
  , rulePrecision
  , ruleRiyals
  ]
