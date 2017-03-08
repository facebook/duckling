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

module Duckling.Finance.Types where

import Control.DeepSeq
import Data.Aeson
import Data.Hashable
import Data.Text (Text)
import GHC.Generics
import Prelude

import Duckling.Resolve (Resolve(..))

data Currency
  -- ambiguous
  = Cent
  | Dollar
  | Pound
  | Unnamed -- e.g. bucks
  -- unambiguous
  | AED
  | AUD
  | BRL
  | EGP
  | EUR
  | GBP
  | IDR
  | INR
  | JPY
  | KRW
  | KWD
  | LBP
  | NOK
  | PTS
  | QAR
  | RON
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
  toJSON Unnamed = "unknown"
  toJSON AED     = "AED"
  toJSON AUD     = "AUD"
  toJSON BRL     = "BRL"
  toJSON EGP     = "EGP"
  toJSON EUR     = "EUR"
  toJSON GBP     = "GBP"
  toJSON IDR     = "IDR"
  toJSON INR     = "INR"
  toJSON JPY     = "JPY"
  toJSON KRW     = "KRW"
  toJSON KWD     = "KWD"
  toJSON LBP     = "LBP"
  toJSON NOK     = "NOK"
  toJSON PTS     = "PTS"
  toJSON QAR     = "QAR"
  toJSON RON     = "RON"
  toJSON SAR     = "SAR"
  toJSON SEK     = "SEK"
  toJSON SGD     = "SGD"
  toJSON USD     = "USD"
  toJSON VND     = "VND"

data FinanceData = FinanceData
  { value    :: Maybe Double
  , currency :: Currency
  }
  deriving (Eq, Generic, Hashable, Show, Ord, NFData)

data FinanceValue = FinanceValue
  { vCurrency :: Currency
  , vValue    :: Double
  }
  deriving (Eq, Show)

instance Resolve FinanceData where
  type ResolvedValue FinanceData = FinanceValue
  resolve _ FinanceData {value = Nothing} = Nothing
  resolve _ FinanceData {value = Just value, currency} =
    Just FinanceValue {vValue = value, vCurrency = currency}

instance ToJSON FinanceValue where
  toJSON FinanceValue {vCurrency, vValue} = object
    [ "type"  .= ("value" :: Text)
    , "value" .= vValue
    , "unit"  .= vCurrency
    ]
