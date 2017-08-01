-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Duckling.Quantity.Types where

import Control.DeepSeq
import Data.Aeson
import Data.Hashable
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics
import Prelude

import Duckling.Resolve (Resolve(..))

data Unit
  = Bowl
  | Cup
  | Custom Text
  | Dish
  | Gram
  | Ounce
  | Pint
  | Pound
  | Quart
  | Tablespoon
  | Teaspoon
  | Unnamed
  deriving (Eq, Generic, Hashable, Ord, Show, NFData)

instance ToJSON Unit where
  toJSON (Custom x) = String $ Text.toLower x
  toJSON x = String . Text.toLower . Text.pack $ show x

data QuantityData = QuantityData
  { unit :: Unit
  , value :: Double
  , product :: Maybe Text
  } deriving (Eq, Generic, Hashable, Ord, Show, NFData)

instance ToJSON QuantityData where
  toJSON (QuantityData unit value product) = object $
    [ "type" .= ("value" :: Text)
    , "value" .= value
    , "unit" .= unit
    ]
    ++ [ "product" .= p | Just p <- [product] ]

instance Resolve QuantityData where
  type ResolvedValue QuantityData = QuantityData
  resolve _ x = Just x
