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

module Duckling.Email.Types where

import Control.DeepSeq
import Data.Aeson
import Data.Hashable
import Data.Text (Text)
import GHC.Generics
import Prelude

import Duckling.Resolve (Resolve(..))

newtype EmailData = EmailData { value :: Text }
  deriving (Eq, Generic, Hashable, Ord, Show, NFData)

instance Resolve EmailData where
  type ResolvedValue EmailData = EmailData
  resolve _ _ x = Just (x, False)

instance ToJSON EmailData where
  toJSON EmailData {value} = object [ "value" .= value ]
