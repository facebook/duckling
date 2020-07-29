-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.MN.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

context :: Context
context = testContext {locale = makeLocale MN Nothing}

corpus :: Corpus
corpus = (context, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple 0)
             [ "0"
             , "нойл"
             , "тэг"
             ]
  , examples (simple 1)
             [ "1"
             , "нэг"
             ]
  , examples (simple 2)
             [ "хоёр"
             ]
  , examples (simple 3)
             [ "гурав"
             ]
  , examples (simple 4)
             [ "дөрөв"
             ]
  , examples (simple 5)
             [ "тав"
             ]
  , examples (simple 6)
             [ "зургаа"
             ]
  , examples (simple 7)
             [ "долоо"
             ]
  , examples (simple 8)
             [ "найм"
             ]
  , examples (simple 9)
             [ "ес"
             ]
  , examples (simple 11)
             [ "арван нэг"
             ]
  , examples (simple 15)
             [ "арван тав"
             ]
  , examples (simple 17)
             [ "арван долоо"
             ]
  , examples (simple 20)
             [ "20"
             , "хорь"
             ]
  , examples (simple 22)
             [ "хорин хоёр"
             ]
  , examples (simple 24)
             [ "24"
             , "хорин дөрөв"
             ]
  , examples (simple 26)
             [ "хорин зургаа"
             ]
  , examples (simple 28)
             [ "хорин найм"
             ]
  , examples (simple 10)
             [ "арав"
             ]
  , examples (simple 20)
             [ "хорь"
             ]
  , examples (simple 50)
             [ "тавь"
             ]
  , examples (simple 34)
             [ "гучин дөрөв"
             ]
  , examples (simple 99)
             [ "ерэн ес"
             ]
  ]
