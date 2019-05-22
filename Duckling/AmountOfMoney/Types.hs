-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


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

import Duckling.Resolve (Resolve(..), Options(..))

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
  | CAD
  | CHF
  | CNY
  | CZK
  | DKK
  | EGP
  | EUR
  | GBP
  | HKD
  | HRK
  | IDR
  | ILS
  | INR
  | IQD
  | JMD
  | JOD
  | JPY
  | GEL
  | KRW
  | KWD
  | LBP
  | MAD
  | MNT
  | MYR
  | NOK
  | NZD
  | PKR
  | PLN
  | PTS
  | QAR
  | RON
  | RUB
  | SAR
  | SEK
  | SGD
  | THB
  | TTD
  | USD
  | VND
  | ZAR
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
  toJSON CAD     = "CAD"
  toJSON CHF     = "CHF"
  toJSON CNY     = "CNY"
  toJSON CZK     = "CZK"
  toJSON DKK     = "DKK"
  toJSON EGP     = "EGP"
  toJSON EUR     = "EUR"
  toJSON GBP     = "GBP"
  toJSON HKD     = "HKD"
  toJSON HRK     = "HRK"
  toJSON IDR     = "IDR"
  toJSON ILS     = "ILS"
  toJSON IQD     = "IQD"
  toJSON INR     = "INR"
  toJSON JMD     = "JMD"
  toJSON JOD     = "JOD"
  toJSON JPY     = "JPY"
  toJSON GEL     = "GEL"
  toJSON KRW     = "KRW"
  toJSON KWD     = "KWD"
  toJSON LBP     = "LBP"
  toJSON MAD     = "MAD"
  toJSON MNT     = "MNT"
  toJSON MYR     = "MYR"
  toJSON NOK     = "NOK"
  toJSON NZD     = "NZD"
  toJSON PTS     = "PTS"
  toJSON PKR     = "PKR"
  toJSON PLN     = "PLN"
  toJSON QAR     = "QAR"
  toJSON RON     = "RON"
  toJSON RUB     = "RUB"
  toJSON SAR     = "SAR"
  toJSON SEK     = "SEK"
  toJSON SGD     = "SGD"
  toJSON THB     = "THB"
  toJSON TTD     = "TTD"
  toJSON USD     = "USD"
  toJSON VND     = "VND"
  toJSON ZAR     = "ZAR"

data AmountOfMoneyData = AmountOfMoneyData
  { value    :: Maybe Double
  , currency :: Currency
  , minValue :: Maybe Double
  , maxValue :: Maybe Double
  , latent   :: Bool
  }
  deriving (Eq, Generic, Hashable, Show, Ord, NFData)

instance Resolve AmountOfMoneyData where
  type ResolvedValue AmountOfMoneyData = AmountOfMoneyValue
  resolve _ Options {withLatent = False} AmountOfMoneyData {latent = True} =
    Nothing
  resolve _ _ AmountOfMoneyData {value = Nothing, minValue = Nothing
                              , maxValue = Nothing} =
    Nothing
  resolve _ _ AmountOfMoneyData {value = Just value, currency, latent} =
    Just (simple currency value, latent)
  resolve _ _ AmountOfMoneyData {value = Nothing, currency = c
                              , minValue = Just from, maxValue = Just to
                              , latent} =
    Just (between c (from, to), latent)
  resolve _ _ AmountOfMoneyData {value = Nothing, currency = c
                              , minValue = Just v, maxValue = Nothing
                              , latent} =
    Just (above c v, latent)
  resolve _ _ AmountOfMoneyData {value = Nothing, currency = c
                              , minValue = Nothing, maxValue = Just v
                              , latent} =
    Just (under c v, latent)

amountOfMoneyData' :: AmountOfMoneyData
amountOfMoneyData' = AmountOfMoneyData
  { value = Nothing
  , currency = Unnamed
  , minValue = Nothing
  , maxValue = Nothing
  , latent = False
  }

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
