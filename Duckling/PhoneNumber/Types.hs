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

module Duckling.PhoneNumber.Types where

import Control.DeepSeq
import Data.Aeson
import Data.Hashable
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics
import qualified TextShow as TS

import Prelude

import Duckling.Resolve (Resolve(..))

data PhoneNumberData = PhoneNumberData
  { prefix :: Maybe Integer
  , number :: Text
  , extension :: Maybe Integer
  }
  deriving (Eq, Generic, Hashable, Ord, Show, NFData)

instance Resolve PhoneNumberData where
  type ResolvedValue PhoneNumberData = PhoneNumberValue
  resolve _ _ PhoneNumberData {prefix, number, extension} = Just
    (PhoneNumberValue {value = Text.concat [p, number, e]}, False)
    where
      p = case prefix of
        Just p -> "(+" <> TS.showt p <> ") "
        Nothing -> ""
      e = case extension of
        Just e -> " ext " <> TS.showt e
        Nothing -> ""

data PhoneNumberValue = PhoneNumberValue { value :: Text }
  deriving (Eq, Ord, Show)

instance ToJSON PhoneNumberValue where
  toJSON (PhoneNumberValue value) = object [ "value" .= value ]
