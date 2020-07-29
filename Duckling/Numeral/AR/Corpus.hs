-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


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
  [ examples (simple 0)
             [ "0"
             , "صفر"
             , "٠"
             , "٠٫٠"
             ]
  , examples (simple 1)
             [ "1"
             , "واحد"
             , "١"
             , "١٫٠"
             ]
  , examples (simple 4)
             [ "4"
             , "أربعة"
             , "أربع"
             , "اربعه"
             , "٤"
             , "٤٫٠"
             ]
  , examples (simple 6)
             [ "6"
             , "ستة"
             , "ست"
             , "سته"
             , "٦"
             , "٦٫٠"
             ]
  , examples (simple 10)
             [ "10"
             , "عشرة"
             , "عشره"
             , "١٠"
             , "١٠٫٠"
             ]
  , examples (simple 11)
             [ "11"
             , "إحدى عشرة"
             , "إحدى عشر"
             , "احد عشر"
             , "١١"
             , "١١٫٠"
             ]
  , examples (simple 12)
             [ "12"
             , "إثنتى عشر"
             , "إثنى عشر"
             , "١٢"
             , "١٢٫٠"
             ]
  , examples (simple 14)
             [ "14"
             , "أربع عشر"
             , "١٤"
             , "١٤٫٠"
             ]
  , examples (simple 16)
             [ "16"
             , "ستة عشر"
             , "١٦"
             , "١٦٫٠"
             ]
  , examples (simple 17)
             [ "17"
             , "سبع عشر"
             , "١٧"
             , "١٧٫٠"
             ]
  , examples (simple 18)
             [ "18"
             , "ثمان عشرة"
             , "١٨"
             , "١٨٫٠"
             ]
  , examples (simple 20)
             [ "عشرون"
             , "عشرين"
             , "٢٠"
             , "٢٠٫٠"
             ]
  , examples (simple 21)
             [ "واحدة و عشرون"
             , "21"
             , "٢١"
             , "٢١٫٠"
             ]
  , examples (simple 24)
             [ "أربعة و عشرون"
             , "24"
             , "٢٤"
             , "٢٤٫٠"
             ]
  , examples (simple 26)
             [ "ستة و عشرون"
             , "26"
             , "٢٦"
             , "٢٦٫٠"
             ]
  , examples (simple 20)
             [ "عشرون"
             , "عشرين"
             , "٢٠"
             ]
  , examples (simple 30)
             [ "ثلاثون"
             , "ثلاثين"
             , "٣٠"
             , "٣٠٫٠"
             ]
  , examples (simple 33)
             [ "33"
             , "ثلاث و ثلاثون"
             , "٣٣"
             , "٣٣٫٠"
             ]
  , examples (simple 40)
             [ "اربعون"
             , "أربعين"
             , "٤٠"
             , "٤٠٫٠"
             ]
  , examples (simple 200)
             [ "مائتين"
             , "مائتان"
             , "٢٠٠"
             , "٢٠٠٫٠"
             ]
  , examples (simple 300)
             [ "ثلاثمائة"
             , "٣٠٠"
             , "٣٠٠٫٠"
             ]
  , examples (simple 350)
             [ "ثلاثمائة وخمسين"
             , "٣٥٠"
             , "٣٥٠٫٠"
             ]
  , examples (simple 500)
             [ "خمسمائة"
             , "٥٠٠"
             , "٥٠٠٫٠"
             ]
  , examples (simple 525)
             [ "خمسمائة و خمسة و عشرون"
             , "525"
             , "٥٢٥"
             , "٥٢٥٫٠"
             ]
  , examples (simple 700)
             [ "سبعمائة"
             , "٧٠٠"
             , "٧٠٠٫٠"
             ]
  , examples (simple 1.1)
             [ "1.1"
             , "1.10"
             , "01.10"
             , "1 فاصلة 1"
             , "واحد فاصلة واحد"
             , "١١/١٠"
             , "١٫١"
             , "١٫١٠"
             ]
  , examples (simple 0.77)
             [ "0.77"
             , ".77"
             , "٧٧/١٠٠"
             , "٫٧٧"
             , "٠٫٧٧"
             , "٠٫٧٧٠"
             ]
  , examples (simple 2000)
             [ "2000"
             , "الفان"
             , "٢٠٠٠"
             , "الفين"
             ]
  , examples (simple 100000)
             [ "100000"
             , "100 الف"
             , "١٠٠٠٠٠"
             , "١٠٠٠٠٠٫٠٠"
             , "١٠٠٬٠٠٠"
             , "١٠٠٬٠٠٠٫٠"
             ]
  , examples (simple 10000)
             [ "10000"
             , "10 آلاف"
             , "١٠٠٠٠"
             , "١٠٠٠٠٫٠٠"
             , "١٠٬٠٠٠"
             , "١٠٬٠٠٠٫٠٠"
             ]
  , examples (simple 1000000)
             [ "1000000"
             , "مليون"
             , "١٠٠٠٠٠٠"
             , "١٠٠٠٠٠٠٫٠٠"
             , "١٬٠٠٠٬٠٠٠"
             , "١٬٠٠٠٬٠٠٠٫٠٠"
             ]
  , examples (simple 2000000)
             [ "2000000"
             , "2 مليون"
             , "مليونان"
             , "مليونين"
             , "٢٠٠٠٠٠٠"
             , "٢٠٠٠٠٠٠٫٠٠"
             , "٢٬٠٠٠٬٠٠٠"
             , "٢٬٠٠٠٬٠٠٠٫٠٠"
             ]
  , examples (simple 3000000)
             [ "3 ملايين"
             , "3000000"
             , "3 مليون"
             , "٣٠٠٠٠٠٠"
             , "٣٠٠٠٠٠٠٫٠٠"
             , "٣٬٠٠٠٬٠٠٠"
             , "٣٬٠٠٠٬٠٠٠٫٠٠"
             ]
  , examples (simple (-1200000))
             [ "-1200000"
             , "-١٢٠٠٠٠٠"
             , "-١٢٠٠٠٠٠٫٠٠"
             , "-١٬٢٠٠٬٠٠٠"
             , "-١٬٢٠٠٬٠٠٠٫٠٠"
             ]
  , examples (simple (-1.2))
             [ "-١٢/١٠"
             , "-1.2"
             , "-١٫٢"
             , "-١٫٢٠"
             ]
  ]
