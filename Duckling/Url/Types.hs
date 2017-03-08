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

module Duckling.Url.Types where

import Control.DeepSeq
import Data.Aeson
import Data.Hashable
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics
import Prelude

import Duckling.Resolve (Resolve(..))

data UrlData = UrlData
  { protocol :: Maybe Text
  , domain :: Text
  , path :: Maybe Text
  , query :: Maybe Text
  }
  deriving (Eq, Generic, Hashable, Ord, Show, NFData)

instance Resolve UrlData where
  type ResolvedValue UrlData = UrlValue
  resolve _ UrlData {protocol, domain, path, query} = Just UrlValue
    {value = p <> domain <> rest}
    where
      p = case protocol of
        Just p -> p <> "://"
        Nothing -> ""
      rest = Text.concat $ catMaybes [path, query]

data UrlValue = UrlValue { value :: Text }
  deriving (Eq, Ord, Show)

instance ToJSON UrlValue where
  toJSON (UrlValue value) = object [ "value" .= value ]
