-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Control.DeepSeq
import Control.Monad
import Data.Aeson
import Data.Hashable
import Data.Semigroup ((<>))
import Data.Some
import Data.Text (Text)
import Data.Typeable
import GHC.Generics
import Prelude
import qualified Data.HashSet as HashSet
import qualified TextShow as TS

import Duckling.Debug
import Duckling.Locale
import Duckling.Resolve (Resolve(..))
import Duckling.Types

data MyDimension = MyDimension deriving (Eq, Show, Typeable)

instance CustomDimension MyDimension where
  type DimensionData MyDimension = MyData
  dimRules _ = [myRule]
  dimLangRules _ _ = []
  dimLocaleRules _ _ = []
  dimDependents _ = HashSet.empty

data MyData = MyData
  { iField :: Int
  , bField :: Bool
  , tField :: Text
  }
  deriving (Eq, Generic, Hashable, Ord, Show, NFData)

instance Resolve MyData where
  type ResolvedValue MyData = MyValue
  resolve _ _ MyData{..} = Just
    ( MyValue $ TS.showt iField <> "," <> TS.showt bField <> "," <> tField
    , False )

newtype MyValue = MyValue { value :: Text }
  deriving (Eq, Ord, Show)

instance ToJSON MyValue where
  toJSON (MyValue value) = object [ "value" .= value ]

myRule :: Rule
myRule = Rule
  { name = "my dimension"
  , pattern =
    [ regex "my dimension"
    ]
  , prod = \case
      (_:_) -> Just . Token (CustomDimension MyDimension) $ MyData
                 { iField = 42
                 , bField = True
                 , tField = "hello world"
                 }
      _ -> Nothing
  }

main :: IO ()
main = do
  let en = makeLocale EN Nothing
  debug en "input for my dimension" [This (CustomDimension MyDimension)] >>= print
