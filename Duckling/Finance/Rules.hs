-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Finance.Rules
  ( rules
  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Data.String
import Data.Text (Text)
import qualified Data.Text as Text
import Prelude

import Duckling.Dimensions.Types
import Duckling.Finance.Helpers
import Duckling.Finance.Types (Currency(..), FinanceData (..))
import qualified Duckling.Finance.Types as TFinance
import Duckling.Number.Types (NumberData (..))
import qualified Duckling.Number.Types     as TNumber
import Duckling.Regex.Types
import Duckling.Types

currencies :: HashMap Text Currency
currencies = HashMap.fromList
  [ ("aed", AED)
  , ("aud", AUD)
  , ("brl", BRL)
  , ("\x00a2", Cent)
  , ("c", Cent)
  , ("$", Dollar)
  , ("dollar", Dollar)
  , ("dollars", Dollar)
  , ("egp", EGP)
  , ("\x20ac", EUR)
  , ("eur", EUR)
  , ("euro", EUR)
  , ("euros", EUR)
  , ("eurs", EUR)
  , ("\x20acur", EUR)
  , ("\x20acuro", EUR)
  , ("\x20acuros", EUR)
  , ("\x20acurs", EUR)
  , ("gbp", GBP)
  , ("idr", IDR)
  , ("inr", INR)
  , ("rs", INR)
  , ("rs.", INR)
  , ("rupee", INR)
  , ("rupees", INR)
  , ("\x00a5", JPY)
  , ("jpy", JPY)
  , ("yen", JPY)
  , ("krw", KRW)
  , ("kwd", KWD)
  , ("lbp", LBP)
  , ("nok", NOK)
  , ("\x00a3", Pound)
  , ("pt", PTS)
  , ("pta", PTS)
  , ("ptas", PTS)
  , ("pts", PTS)
  , ("qar", QAR)
  , ("ron", RON)
  , ("sar", SAR)
  , ("sek", SEK)
  , ("sgd", SGD)
  , ("usd", USD)
  , ("us$", USD)
  , ("vnd", VND)
  ]

ruleCurrencies :: Rule
ruleCurrencies = Rule
  { name = "currencies"
  , pattern =
    [ regex "(aed|aud|brl|\x00a2|c|\\$|dollars?|egp|(e|\x20ac)uro?s?|\x20ac|gbp|idr|inr|\x00a5|jpy|krw|kwd|lbp|nok|\x00a3|pta?s?|qar|rs\\.?|ron|rupees?|sar|sek|sgb|us(d|\\$)|vnd|yen)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        c <- HashMap.lookup (Text.toLower match) currencies
        Just . Token Finance $ currencyOnly c
      _ -> Nothing
  }

ruleAmountUnit :: Rule
ruleAmountUnit = Rule
  { name = "<amount> <unit>"
  , pattern =
    [ dimension DNumber
    , financeWith TFinance.value isNothing
    ]
  , prod = \tokens -> case tokens of
      (Token DNumber (NumberData {TNumber.value = v}):
       Token Finance (FinanceData {TFinance.currency = c}):
       _) -> Just . Token Finance . withValue v $ currencyOnly c
      _ -> Nothing
  }

ruleUnitAmount :: Rule
ruleUnitAmount = Rule
  { name = "<unit> <amount>"
  , pattern =
    [ financeWith TFinance.value isNothing
    , dimension DNumber
    ]
  , prod = \tokens -> case tokens of
      (Token Finance (FinanceData {TFinance.currency = c}):
       Token DNumber (NumberData {TNumber.value = v}):
       _) -> Just . Token Finance . withValue v $ currencyOnly c
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleAmountUnit
  , ruleCurrencies
  , ruleUnitAmount
  ]
