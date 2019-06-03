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
  [ examples (NumeralValue 0)
             [ "0"
             , "нойл"
             , "тэг"
             ]
  , examples (NumeralValue 1)
             [ "1"
             , "нэг"
             ]
  , examples (NumeralValue 2)
             [ "хоёр"
             ]
  , examples (NumeralValue 3)
             [ "гурав"
             ]
  , examples (NumeralValue 4)
             [ "дөрөв"
             ]
  , examples (NumeralValue 5)
             [ "тав"
             ]
  , examples (NumeralValue 6)
             [ "зургаа"
             ]
  , examples (NumeralValue 7)
             [ "долоо"
             ]
  , examples (NumeralValue 8)
             [ "найм"
             ]
  , examples (NumeralValue 9)
             [ "ес"
             ]
  , examples (NumeralValue 11)
             [ "арван нэг"
             ]
  , examples (NumeralValue 15)
             [ "арван тав"
             ]
  , examples (NumeralValue 17)
             [ "арван долоо"
             ]
  , examples (NumeralValue 20)
             [ "20"
             , "хорь"
             ]
  , examples (NumeralValue 22)
             [ "хорин хоёр"
             ]
  , examples (NumeralValue 24)
             [ "24"
             , "хорин дөрөв"
             ]
  , examples (NumeralValue 26)
             [ "хорин зургаа"
             ]
  , examples (NumeralValue 28)
             [ "хорин найм"
             ]
  , examples (NumeralValue 10)
             [ "арав"
             ]
  , examples (NumeralValue 20)
             [ "хорь"
             ]
  , examples (NumeralValue 50)
             [ "тавь"
             ]
  , examples (NumeralValue 34)
             [ "гучин дөрөв"
             ]
  , examples (NumeralValue 99)
             [ "ерэн ес"
             ]
  ]
