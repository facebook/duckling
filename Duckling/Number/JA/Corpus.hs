-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Number.JA.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Lang
import Duckling.Number.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {lang = JA}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumberValue 0)
             [ "0"
             , "零"
             , "ゼロ"
             ]
  , examples (NumberValue 1)
             [ "1"
             , "一"
             ]
  , examples (NumberValue 33)
             [ "33"
             , "三十三"
             , "0033"
             ]
  , examples (NumberValue 14)
             [ "14"
             , "十四"
             ]
  , examples (NumberValue 16)
             [ "16"
             , "十六"
             ]
  , examples (NumberValue 17)
             [ "17"
             , "十七"
             ]
  , examples (NumberValue 18)
             [ "18"
             , "十八"
             ]
  , examples (NumberValue 100)
             [ "100"
             , "百"
             ]
  , examples (NumberValue 101)
             [ "101"
             , "百一"
             ]
  , examples (NumberValue 200)
             [ "200"
             ]
  , examples (NumberValue 1.1)
             [ "1.1"
             , "1.10"
             , "01.10"
             ]
  , examples (NumberValue 0.77)
             [ "0.77"
             , ".77"
             ]
  , examples (NumberValue 100000)
             [ "100,000"
             , "100000"
             , "100K"
             , "100k"
             ]
  , examples (NumberValue 3000000)
             [ "3M"
             , "3000K"
             , "3000000"
             , "3,000,000"
             ]
  , examples (NumberValue 1200000)
             [ "1,200,000"
             , "1200000"
             , "1.2M"
             , "1200K"
             , ".0012G"
             ]
  , examples (NumberValue (-1200000))
             [ "- 1,200,000"
             , "-1200000"
             , "-1.2M"
             , "-1200K"
             , "-.0012G"
             ]
  , examples (NumberValue 5000)
             [ "5千"
             ]
  , examples (NumberValue 20000)
             [ "2万"
             ]
  ]
