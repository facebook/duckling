-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.AR.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale AR Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumeralValue 0)
             [ "0"
             , "صفر"
             , "٠"
             ]
  , examples (NumeralValue 1)
             [ "1"
             , "واحد"
             , "١"
             ]
  , examples (NumeralValue 4)
             [ "4"
             , "أربعة"
             , "أربع"
             , "اربعه"
             , "٤"
             ]
  , examples (NumeralValue 6)
             [ "6"
             , "ستة"
             , "ست"
             , "سته"
             , "٦"
             ]
  , examples (NumeralValue 10)
             [ "10"
             , "عشرة"
             , "عشره"
             , "١٠"
             ]
  , examples (NumeralValue 11)
             [ "11"
             , "إحدى عشرة"
             , "إحدى عشر"
             , "احد عشر"
             , "١١"
             ]
  , examples (NumeralValue 12)
             [ "12"
             , "إثنتى عشر"
             , "إثنى عشر"
             , "١٢"
             ]
  , examples (NumeralValue 14)
             [ "14"
             , "أربع عشر"
             , "١٤"
             ]
  , examples (NumeralValue 16)
             [ "16"
             , "ستة عشر"
             , "١٦"
             ]
  , examples (NumeralValue 17)
             [ "17"
             , "سبع عشر"
             , "١٧"
             ]
  , examples (NumeralValue 18)
             [ "18"
             , "ثمان عشرة"
             , "١٨"
             ]
  , examples (NumeralValue 20)
             [ "عشرون"
             , "عشرين"
             , "٢٠"
             ]
  , examples (NumeralValue 21)
             [ "واحدة و عشرون"
             , "21"
             , "٢١"
             ]
  , examples (NumeralValue 24)
             [ "أربعة و عشرون"
             , "24"
             , "٢٤"
             ]
  , examples (NumeralValue 26)
             [ "ستة و عشرون"
             , "26"
             , "٢٦"
             ]
  , examples (NumeralValue 20)
             [ "عشرون"
             , "عشرين"
             , "٢٠"
             ]
  , examples (NumeralValue 30)
             [ "ثلاثون"
             , "ثلاثين"
             , "٣٠"
             ]
  , examples (NumeralValue 33)
             [ "33"
             , "ثلاث و ثلاثون"
             , "٣٣"
             ]
  , examples (NumeralValue 40)
             [ "اربعون"
             , "أربعين"
             , "٤٠"
             ]
  , examples (NumeralValue 200)
             [ "مائتين"
             , "مائتان"
             , "٢٠٠"
             ]
  , examples (NumeralValue 300)
             [ "ثلاثمائة"
             , "٣٠٠"
             ]
  , examples (NumeralValue 350)
             [ "ثلاثمائة وخمسين"
             , "٣٥٠"
             ]
  , examples (NumeralValue 500)
             [ "خمسمائة"
             , "٥٠٠"
             ]
  , examples (NumeralValue 525)
             [ "خمسمائة و خمسة و عشرون"
             , "525"
             , "٥٢٥"
             ]
  , examples (NumeralValue 700)
             [ "سبعمائة"
             , "٧٠٠"
             ]
  , examples (NumeralValue 1.1)
             [ "1.1"
             , "1.10"
             , "01.10"
             , "1 فاصلة 1"
             , "واحد فاصلة واحد"
             , "١١/١٠"
             ]
  , examples (NumeralValue 0.77)
             [ "0.77"
             , ".77"
             , "٧٧/١٠٠"
             ]
  , examples (NumeralValue 2000)
             [ "2000"
             , "الفان"
             , "٢٠٠٠"
             , "الفين"
             ]
  , examples (NumeralValue 100000)
             [ "100000"
             , "100 الف"
             , "١٠٠٠٠٠"
             ]
  , examples (NumeralValue 10000)
             [ "10000"
             , "10 آلاف"
             , "١٠٠٠٠"
             ]
  , examples (NumeralValue 1000000)
             [ "1000000"
             , "مليون"
             , "١٠٠٠٠٠٠"
             ]
  , examples (NumeralValue 2000000)
             [ "2000000"
             , "2 مليون"
             , "مليونان"
             , "مليونين"
             , "٢٠٠٠٠٠٠"
             ]
  , examples (NumeralValue 3000000)
             [ "3 ملايين"
             , "3000000"
             , "3 مليون"
             , "٣٠٠٠٠٠٠"
             ]
  , examples (NumeralValue (-1200000))
             [ "-1200000"
             , "-١٢٠٠٠٠٠"
             ]
  , examples (NumeralValue (-1.2))
             [ "-١٢/١٠"
             , "-1.2"
             ]
  ]
