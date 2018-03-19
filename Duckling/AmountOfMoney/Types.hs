-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Duckling.AmountOfMoney.Types where

import Control.DeepSeq
import Data.Aeson
import Data.Hashable
import Data.Text (Text)
import GHC.Generics
import Prelude
import qualified Data.HashMap.Strict as H

import Duckling.Resolve (Resolve(..))

data Currency
  -- ambiguous
  = Cent
  | Dinar
  | Dirham
  | Dollar
  | Pound
  | Rial
  | Riyal
  | Unnamed -- e.g. bucks
  -- unambiguous
  | AED
  | AUD
  | BGN
  | BRL
  | BYN
  | EGP
  | EUR
  | GBP
  | HRK
  | IDR
  | ILS
  | INR
  | IQD
  | JOD
  | JPY
  | KRW
  | KWD
  | LBP
  | MAD
  | MYR
  | NOK
  | PTS
  | QAR
  | RON
  | RUB
  | SAR
  | SEK
  | SGD
  | USD
  | VND
  deriving (Eq, Generic, Hashable, Show, Ord, NFData)

instance ToJSON Currency where
  toJSON Cent    = "cent"
  toJSON Dollar  = "$"
  toJSON Pound   = "\x00a3"
  toJSON Dinar   = "dinar"
  toJSON Dirham  = "dirham"
  toJSON Rial    = "rial"
  toJSON Riyal   = "riyal"
  toJSON Unnamed = "unknown"
  toJSON AED     = "AED"
  toJSON AUD     = "AUD"
  toJSON BGN     = "BGN"
  toJSON BRL     = "BRL"
  toJSON BYN     = "BYN"
  toJSON EGP     = "EGP"
  toJSON EUR     = "EUR"
  toJSON GBP     = "GBP"
  toJSON HRK     = "HRK"
  toJSON IDR     = "IDR"
  toJSON ILS     = "ILS"
  toJSON IQD     = "IQD"
  toJSON INR     = "INR"
  toJSON JOD     = "JOD"
  toJSON JPY     = "JPY"
  toJSON KRW     = "KRW"
  toJSON KWD     = "KWD"
  toJSON LBP     = "LBP"
  toJSON MAD     = "MAD"
  toJSON MYR     = "MYR"
  toJSON NOK     = "NOK"
  toJSON PTS     = "PTS"
  toJSON QAR     = "QAR"
  toJSON RON     = "RON"
  toJSON RUB     = "RUB"
  toJSON SAR     = "SAR"
  toJSON SEK     = "SEK"
  toJSON SGD     = "SGD"
  toJSON USD     = "USD"
  toJSON VND     = "VND"

data AmountOfMoneyData = AmountOfMoneyData
  { value    :: Maybe Double
  , currency :: Currency
  , minValue :: Maybe Double
  , maxValue :: Maybe Double
  }
  deriving (Eq, Generic, Hashable, Show, Ord, NFData)

instance Resolve AmountOfMoneyData where
  type ResolvedValue AmountOfMoneyData = AmountOfMoneyValue
  resolve _ _ AmountOfMoneyData {value = Nothing, minValue = Nothing
                              , maxValue = Nothing} = Nothing
  resolve _ _ AmountOfMoneyData {value = Just value, currency} =
    Just (simple currency value, False)
  resolve _ _ AmountOfMoneyData {value = Nothing, currency = c
                              , minValue = Just from, maxValue = Just to} =
    Just (between c (from, to), False)
  resolve _ _ AmountOfMoneyData {value = Nothing, currency = c
                              , minValue = Just v, maxValue = Nothing} =
    Just (above c v, False)
  resolve _ _ AmountOfMoneyData {value = Nothing, currency = c
                              , minValue = Nothing, maxValue = Just v} =
    Just (under c v, False)

data IntervalDirection = Above | Under
  deriving (Eq, Generic, Hashable, Ord, Show, NFData)

data SingleValue = SingleValue
  { vCurrency :: Currency
  , vValue    :: Double
  }
  deriving (Eq, Show)

instance ToJSON SingleValue where
  toJSON SingleValue {vCurrency, vValue} = object
    [ "value" .= vValue
    , "unit"  .= vCurrency
    ]

data AmountOfMoneyValue
  = SimpleValue SingleValue
  | IntervalValue (SingleValue, SingleValue)
  | OpenIntervalValue (SingleValue, IntervalDirection)
  deriving (Show, Eq)

instance ToJSON AmountOfMoneyValue where
  toJSON (SimpleValue value) = case toJSON value of
    Object o -> Object $ H.insert "type" (toJSON ("value" :: Text)) o
    _ -> Object H.empty
  toJSON (IntervalValue (from, to)) = object
    [ "type" .= ("interval" :: Text)
    , "from" .= toJSON from
    , "to" .= toJSON to
    ]
  toJSON (OpenIntervalValue (from, Above)) = object
    [ "type" .= ("interval" :: Text)
    , "from" .= toJSON from
    ]
  toJSON (OpenIntervalValue (to, Under)) = object
    [ "type" .= ("interval" :: Text)
    , "to" .= toJSON to
    ]

-- -----------------------------------------------------------------
-- Value helpers

simple :: Currency -> Double -> AmountOfMoneyValue
simple c v = SimpleValue $ single c v

between :: Currency -> (Double, Double) -> AmountOfMoneyValue
between c (from, to) = IntervalValue (single c from, single c to)

above :: Currency -> Double -> AmountOfMoneyValue
above = openInterval Above

under :: Currency -> Double -> AmountOfMoneyValue
under = openInterval Under

openInterval :: IntervalDirection -> Currency -> Double -> AmountOfMoneyValue
openInterval direction c v = OpenIntervalValue (single c v, direction)

single :: Currency -> Double -> SingleValue
single c v = SingleValue {vCurrency = c, vValue = v}
