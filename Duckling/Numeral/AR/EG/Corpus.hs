-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.AR.EG.Corpus (allExamples) where

import Data.String
import Prelude

import Duckling.Numeral.Types
import Duckling.Testing.Types

allExamples :: [Example]
allExamples = concat
  [ examples (NumeralValue 0)
             [ "0"
             , "صفر"
             , "٠"
             , "٠٫٠"
             ]
  , examples (NumeralValue 1)
             [ "1"
             , "واحد"
             , "١"
             , "١٫٠"
             ]
  , examples (NumeralValue 4)
             [ "4"
             , "أربع"
             , "اربعه"
             , "٤"
             , "٤٫٠"
             ]
  , examples (NumeralValue 6)
             [ "6"
             , "ستة"
             , "ست"
             , "سته"
             , "٦"
             , "٦٫٠"
             ]
  , examples (NumeralValue 10)
             [ "10"
             , "عشرة"
             , "عشره"
             , "١٠"
             , "١٠٫٠"
             ]
  , examples (NumeralValue 11)
             [ "11"
             , "حداشر"
             , "١١"
             , "١١٫٠"
             ]
  , examples (NumeralValue 12)
             [ "12"
             , "إتناشر"
             , "١٢"
             , "١٢٫٠"
             ]
  , examples (NumeralValue 14)
             [ "14"
             , "اربعتاشر"
             , "١٤"
             , "١٤٫٠"
             ]
  , examples (NumeralValue 16)
             [ "16"
             , "ستاشر"
             , "١٦"
             , "١٦٫٠"
             ]
  , examples (NumeralValue 17)
             [ "17"
             , "سبعتاشر"
             , "١٧"
             , "١٧٫٠"
             ]
  , examples (NumeralValue 18)
             [ "18"
             , "تمنتاشر"
             , "١٨"
             , "١٨٫٠"
             ]
  , examples (NumeralValue 20)
             [ "عشرين"
             , "٢٠"
             , "٢٠٫٠"
             ]
  , examples (NumeralValue 21)
             [ "واحد و عشرين"
             , "21"
             , "٢١"
             , "٢١٫٠"
             ]
  , examples (NumeralValue 24)
             [ "أربعة و عشرين"
             , "24"
             , "٢٤"
             , "٢٤٫٠"
             ]
  , examples (NumeralValue 26)
             [ "ستة و عشرين"
             , "26"
             , "٢٦"
             , "٢٦٫٠"
             ]
  , examples (NumeralValue 20)
             [ "عشرين"
             , "٢٠"
             ]
  , examples (NumeralValue 30)
             [ "ثلاثين"
             , "٣٠"
             , "٣٠٫٠"
             ]
  , examples (NumeralValue 33)
             [ "33"
             , "تلاتة و تلاتين"
             , "٣٣"
             , "٣٣٫٠"
             ]
  , examples (NumeralValue 40)
             [ "أربعين"
             , "٤٠"
             , "٤٠٫٠"
             ]
  , examples (NumeralValue 200)
             [ "متين"
             , "٢٠٠"
             , "٢٠٠٫٠"
             ]
  , examples (NumeralValue 210)
             [ "متين وعشرة"
             , "٢١٠"
             , "٢١٠٫٠"
             ]
  , examples (NumeralValue 221)
             [ "متين واحد وعشرين"
             , "٢٢١"
             , "٢٢١٫٠"
             ]
  , examples (NumeralValue 263)
             [ "متين تلاتة وستين"
             , "٢٦٣"
             , "٢٦٣٫٠"
             ]
  , examples (NumeralValue 300)
             [ "تلتمية"
             , "٣٠٠"
             , "٣٠٠٫٠"
             ]
  , examples (NumeralValue 350)
             [ "تلتمية وخمسين"
             , "٣٥٠"
             , "٣٥٠٫٠"
             ]
  , examples (NumeralValue 500)
             [ "خمسمية"
             , "٥٠٠"
             , "٥٠٠٫٠"
             ]
  , examples (NumeralValue 525)
             [ "خمسمية خمسة و عشرين"
             , "525"
             , "٥٢٥"
             , "٥٢٥٫٠"
             ]
  , examples (NumeralValue 700)
             [ "سبعمية"
             , "٧٠٠"
             , "٧٠٠٫٠"
             ]
  , examples (NumeralValue 1.1)
             [ "1.1"
             , "1.10"
             , "01.10"
             , "1 فاصلة 1"
             , "واحد فاصلة واحد"
             , "١١/١٠"
             , "١٫١"
             , "١٫١٠"
             ]
  , examples (NumeralValue 0.77)
             [ "0.77"
             , ".77"
             , "٧٧/١٠٠"
             , "٫٧٧"
             , "٠٫٧٧"
             , "٠٫٧٧٠"
             ]
  , examples (NumeralValue 2000)
             [ "2000"
             , "٢٠٠٠"
             , "الفين"
             ]
  , examples (NumeralValue 100000)
             [ "100000"
             , "100 الف"
             , "١٠٠٠٠٠"
             , "١٠٠٠٠٠٫٠٠"
             , "١٠٠٬٠٠٠"
             , "١٠٠٬٠٠٠٫٠"
             ]
  , examples (NumeralValue 10000)
             [ "10000"
             , "10 آلاف"
             , "١٠٠٠٠"
             , "١٠٠٠٠٫٠٠"
             , "١٠٬٠٠٠"
             , "١٠٬٠٠٠٫٠٠"
             ]
  , examples (NumeralValue 1000000)
             [ "1000000"
             , "مليون"
             , "١٠٠٠٠٠٠"
             , "١٠٠٠٠٠٠٫٠٠"
             , "١٬٠٠٠٬٠٠٠"
             , "١٬٠٠٠٬٠٠٠٫٠٠"
             ]
  , examples (NumeralValue 2000000)
             [ "2000000"
             , "2 مليون"
             , "اتنين مليون"
             , "٢٠٠٠٠٠٠"
             , "٢٠٠٠٠٠٠٫٠٠"
             , "٢٬٠٠٠٬٠٠٠"
             , "٢٬٠٠٠٬٠٠٠٫٠٠"
             ]
  , examples (NumeralValue 3000000)
             [ "3 ملايين"
             , "3000000"
             , "3 مليون"
             , "٣٠٠٠٠٠٠"
             , "٣٠٠٠٠٠٠٫٠٠"
             , "٣٬٠٠٠٬٠٠٠"
             , "٣٬٠٠٠٬٠٠٠٫٠٠"
             ]
  , examples (NumeralValue (-1200000))
             [ "-1200000"
             , "-١٢٠٠٠٠٠"
             , "-١٢٠٠٠٠٠٫٠٠"
             , "-١٬٢٠٠٬٠٠٠"
             , "-١٬٢٠٠٬٠٠٠٫٠٠"
             ]
  , examples (NumeralValue (-1.2))
             [ "-١٢/١٠"
             , "-1.2"
             , "-١٫٢"
             , "-١٫٢٠"
             ]
  ]
