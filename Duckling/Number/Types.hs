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

module Duckling.Number.Types where

import Control.DeepSeq
import Data.Aeson
import Data.Hashable
import Data.Maybe
import Data.Text (Text)
import GHC.Generics
import Prelude

import Duckling.Resolve

data NumberData = NumberData
  { value        :: Double
  , grain        :: Maybe Int
  , multipliable :: Bool
  }
  deriving (Eq, Generic, Hashable, Ord, Show, NFData)

instance Resolve NumberData where
  type ResolvedValue NumberData = NumberValue
  resolve _ NumberData {value} = Just NumberValue {vValue = value}

newtype NumberValue = NumberValue { vValue :: Double }
  deriving (Eq, Show)

instance ToJSON NumberValue where
  toJSON (NumberValue value) = object
    [ "type" .= ("value" :: Text)
    , "value" .= value
    ]

getIntValue :: Double -> Maybe Int
getIntValue x = if rest == 0 then Just int else Nothing
  where
    (int, rest) = properFraction x :: (Int, Double)

isInteger :: Double -> Bool
isInteger = isJust . getIntValue

isNatural :: Double -> Bool
isNatural x = isInteger x && x > 0

isIntegerBetween :: Double -> Int -> Int -> Bool
isIntegerBetween x low high = case getIntValue x of
  Just int -> low <= int && int <= high
  Nothing -> False
