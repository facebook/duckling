-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.Rules
  ( rules
  ) where

import Data.HashMap.Strict (HashMap)
import Data.Maybe
import Data.String
import Data.Text (Text)
import Prelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Duckling.AmountOfMoney.Helpers
import Duckling.AmountOfMoney.Types (Currency(..), AmountOfMoneyData (..))
import Duckling.Dimensions.Types
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Regex.Types
import Duckling.Types
import qualified Duckling.AmountOfMoney.Types as TAmountOfMoney
import qualified Duckling.Numeral.Types as TNumeral

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
  , ("hrk", HRK)
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
  , ("myr", MYR)
  , ("rm", MYR)
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
    [ regex "(aed|aud|brl|\x00a2|c|\\$|dollars?|egp|(e|\x20ac)uro?s?|\x20ac|gbp|hrk|idr|inr|\x00a5|jpy|krw|kwd|lbp|myr|rm|nok|\x00a3|pta?s?|qar|rs\\.?|ron|rupees?|sar|sek|sgb|us(d|\\$)|vnd|yen)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        c <- HashMap.lookup (Text.toLower match) currencies
        Just . Token AmountOfMoney $ currencyOnly c
      _ -> Nothing
  }

ruleAmountUnit :: Rule
ruleAmountUnit = Rule
  { name = "<amount> <unit>"
  , pattern =
    [ dimension Numeral
    , financeWith TAmountOfMoney.value isNothing
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = v}):
       Token AmountOfMoney (AmountOfMoneyData {TAmountOfMoney.currency = c}):
       _) -> Just . Token AmountOfMoney . withValue v $ currencyOnly c
      _ -> Nothing
  }

ruleUnitAmount :: Rule
ruleUnitAmount = Rule
  { name = "<unit> <amount>"
  , pattern =
    [ financeWith TAmountOfMoney.value isNothing
    , dimension Numeral
    ]
  , prod = \tokens -> case tokens of
      (Token AmountOfMoney (AmountOfMoneyData {TAmountOfMoney.currency = c}):
       Token Numeral (NumeralData {TNumeral.value = v}):
       _) -> Just . Token AmountOfMoney . withValue v $ currencyOnly c
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleAmountUnit
  , ruleCurrencies
  , ruleUnitAmount
  ]
