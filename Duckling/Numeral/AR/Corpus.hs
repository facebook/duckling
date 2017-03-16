-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.AR.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Lang
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {lang = AR}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumeralValue 0)
             [ "0"
             , "صفر"
             ]
  , examples (NumeralValue 1)
             [ "1"
             , "واحد"
             ]
  , examples (NumeralValue 4)
             [ "4"
             , "أربعة"
             , "أربع"
             ]
  , examples (NumeralValue 6)
             [ "6"
             , "ستة"
             , "ست"
             ]
  , examples (NumeralValue 33)
             [ "33"
             , "ثلاث و ثلاثون"
             ]
  , examples (NumeralValue 11)
             [ "11"
             , "إحدى عشرة"
             , "إحدى عشر"
             ]
  , examples (NumeralValue 12)
             [ "12"
             , "إثنتى عشر"
             , "إثنى عشر"
             ]
  , examples (NumeralValue 14)
             [ "14"
             , "أربع عشر"
             ]
  , examples (NumeralValue 16)
             [ "16"
             , "ستة عشر"
             ]
  , examples (NumeralValue 17)
             [ "17"
             , "سبع عشر"
             ]
  , examples (NumeralValue 18)
             [ "18"
             , "ثمان عشرة"
             ]
  , examples (NumeralValue 525)
             [ "خمسمائة و خمسة و عشرون"
             , "525"
             ]
  , examples (NumeralValue 21)
             [ "واحدة و عشرون"
             , "21"
             ]
  , examples (NumeralValue 24)
             [ "أربعة و عشرون"
             , "24"
             ]
  , examples (NumeralValue 26)
             [ "ستة و عشرون"
             , "26"
             ]
  , examples (NumeralValue 1.1)
             [ "1.1"
             , "1.10"
             , "01.10"
             , "1 فاصلة 1"
             , "واحد فاصلة واحد"
             ]
  , examples (NumeralValue 0.77)
             [ "0.77"
             , ".77"
             ]
  , examples (NumeralValue 100000)
             [ "100000"
             , "100 الف"
             ]
  , examples (NumeralValue 10000)
             [ "10000"
             , "10 آلاف"
             ]
  , examples (NumeralValue 3000000)
             [ "3 ملايين"
             , "3000000"
             ]
  , examples (NumeralValue (-1200000))
             [ "-1200000"
             ]
  ]
