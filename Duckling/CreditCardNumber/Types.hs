-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Duckling.CreditCardNumber.Types where

import Control.DeepSeq
import Data.Aeson
import Data.Hashable
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics

import Prelude

import Duckling.Resolve (Resolve(..))

data Issuer
  = Visa
  | Amex
  | Discover
  | Mastercard
  | DinerClub
  | Other
  deriving (Eq, Generic, Hashable, Ord, Show, NFData)

instance ToJSON Issuer where
  toJSON = String . Text.toLower . Text.pack . show

data CreditCardNumberData = CreditCardNumberData
  { number :: Text
  , issuer :: Issuer
  }
  deriving (Eq, Generic, Hashable, Ord, Show, NFData)

instance Resolve CreditCardNumberData where
  type ResolvedValue CreditCardNumberData = CreditCardNumberValue
  resolve _ _ CreditCardNumberData {number, issuer} =
    Just (CreditCardNumberValue number issuer, False)

data CreditCardNumberValue = CreditCardNumberValue
  { vNumber :: Text
  , vIssuer :: Issuer
  }
  deriving (Eq, Ord, Show)

instance ToJSON CreditCardNumberValue where
  toJSON (CreditCardNumberValue number issuer) =
    object [ "value" .= number
           , "issuer" .= issuer
           ]
