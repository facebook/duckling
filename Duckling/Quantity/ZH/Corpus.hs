-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Quantity.ZH.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Quantity.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale ZH Nothing},
  testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Gram 2 Nothing)
             [ "2克"
             , "二克"
             , "两克"
             , "2g"
             , "2.0g"
             ]
  , examples (simple Gram 1500 Nothing)
             [ "3斤"
             , "三斤"
             , "1.5公斤"
             , "1.5千克"
             , "1.5kg"
             , "1500g"
             ]
  , examples (simple Gram 0.035 Nothing)
             [ "35毫克"
             , "三十五毫克"
             , "35mg"
             ]
  , examples (simple Gram 1750 Nothing)
             [ "三斤半"
             , "3斤半"
             , "3斤5两"
             , "1.75千克"
             ]
  , examples (simple Gram 475 Nothing)
             [ "9两半"
             , "九兩半"
             ]
  , examples (simple Gram 3400 Nothing)
             [ "六斤八两"
             , "六斤8兩"
             , "3.4公斤"
             ]
  ]
