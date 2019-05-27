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

module Duckling.Url.Types where

import Control.DeepSeq
import Data.Aeson
import Data.Hashable
import Data.Maybe
import Data.Text (Text)
import GHC.Generics
import Prelude

import Duckling.Resolve (Resolve(..))

data UrlData = UrlData
  { value :: Text
  , domain :: Text
  }
  deriving (Eq, Generic, Hashable, Ord, Show, NFData)

instance Resolve UrlData where
  type ResolvedValue UrlData = UrlData
  resolve _ _ x = Just (x, False)

instance ToJSON UrlData where
  toJSON (UrlData value domain) = object
    [ "value"  .= value
    , "domain" .= domain
    ]
