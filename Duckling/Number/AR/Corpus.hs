-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Number.AR.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Lang
import Duckling.Number.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {lang = AR}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumberValue 0)
             [ "0"
             , "صفر"
             ]
  , examples (NumberValue 1)
             [ "1"
             , "واحد"
             ]
  , examples (NumberValue 4)
             [ "4"
             , "أربعة"
             , "أربع"
             ]
  , examples (NumberValue 6)
             [ "6"
             , "ستة"
             , "ست"
             ]
  , examples (NumberValue 33)
             [ "33"
             , "ثلاث و ثلاثون"
             ]
  , examples (NumberValue 11)
             [ "11"
             , "إحدى عشرة"
             , "إحدى عشر"
             ]
  , examples (NumberValue 12)
             [ "12"
             , "إثنتى عشر"
             , "إثنى عشر"
             ]
  , examples (NumberValue 14)
             [ "14"
             , "أربع عشر"
             ]
  , examples (NumberValue 16)
             [ "16"
             , "ستة عشر"
             ]
  , examples (NumberValue 17)
             [ "17"
             , "سبع عشر"
             ]
  , examples (NumberValue 18)
             [ "18"
             , "ثمان عشرة"
             ]
  , examples (NumberValue 525)
             [ "خمسمائة و خمسة و عشرون"
             , "525"
             ]
  , examples (NumberValue 21)
             [ "واحدة و عشرون"
             , "21"
             ]
  , examples (NumberValue 24)
             [ "أربعة و عشرون"
             , "24"
             ]
  , examples (NumberValue 26)
             [ "ستة و عشرون"
             , "26"
             ]
  , examples (NumberValue 1.1)
             [ "1.1"
             , "1.10"
             , "01.10"
             , "1 فاصلة 1"
             , "واحد فاصلة واحد"
             ]
  , examples (NumberValue 0.77)
             [ "0.77"
             , ".77"
             ]
  , examples (NumberValue 100000)
             [ "100000"
             , "100 الف"
             ]
  , examples (NumberValue 10000)
             [ "10000"
             , "10 آلاف"
             ]
  , examples (NumberValue 3000000)
             [ "3 ملايين"
             , "3000000"
             ]
  , examples (NumberValue (-1200000))
             [ "-1200000"
             ]
  ]
