-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.JA.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale JA Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple 0)
             [ "0"
             , "零"
             , "ゼロ"
             ]
  , examples (simple 1)
             [ "1"
             , "一"
             ]
  , examples (simple 33)
             [ "33"
             , "三十三"
             , "0033"
             ]
  , examples (simple 14)
             [ "14"
             , "十四"
             ]
  , examples (simple 16)
             [ "16"
             , "十六"
             ]
  , examples (simple 17)
             [ "17"
             , "十七"
             ]
  , examples (simple 18)
             [ "18"
             , "十八"
             ]
  , examples (simple 100)
             [ "100"
             , "百"
             ]
  , examples (simple 101)
             [ "101"
             , "百一"
             ]
  , examples (simple 200)
             [ "200"
             ]
  , examples (simple 1.1)
             [ "1.1"
             , "1.10"
             , "01.10"
             ]
  , examples (simple 0.77)
             [ "0.77"
             , ".77"
             ]
  , examples (simple 100000)
             [ "100,000"
             , "100000"
             , "100K"
             , "100k"
             ]
  , examples (simple 3000000)
             [ "3M"
             , "3000K"
             , "3000000"
             , "3,000,000"
             ]
  , examples (simple 1200000)
             [ "1,200,000"
             , "1200000"
             , "1.2M"
             , "1200K"
             , ".0012G"
             ]
  , examples (simple (-1200000))
             [ "- 1,200,000"
             , "-1200000"
             , "-1.2M"
             , "-1200K"
             , "-.0012G"
             ]
  , examples (simple 5000)
             [ "5千"
             ]
  , examples (simple 20000)
             [ "2万"
             ]
  ]
