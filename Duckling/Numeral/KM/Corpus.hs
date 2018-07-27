-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.KM.Corpus
  ( corpus ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {lang = KM}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumeralValue 0)
             [ "០"
             , "សូន្យ"
             ]
  , examples (NumeralValue 1)
             [ "១"
             , "មួយ"
             ]
  , examples (NumeralValue 2)
             [ "២"
             , "ពីរ"
             ]
  , examples (NumeralValue 3)
             [ "៣"
             , "បី"             
             ]
  , examples (NumeralValue 4)
             [ "៤"
             , "បួន"
             ]
  , examples (NumeralValue 5)
             [ "៥"
             , "ប្រាំ"
             ]
  , examples (NumeralValue 6)
             [ "៦"
             , "ប្រាំមួយ"
             ]
  , examples (NumeralValue 7)
             [ "៧"
             , "ប្រាំពីរ"
             , "ប្រាំពិល"
             ]
  , examples (NumeralValue 8)
             [ "៨"
             , "ប្រាំបី"
             ]
  , examples (NumeralValue 9)
             [ "៩"
             , "ប្រាំបួន"
             ]
  , examples (NumeralValue 10)
             [ "១០"
             , "ដប់"
             ]
  , examples (NumeralValue 11)
             [ "១១"
             , "ដប់មួយ"
             ]
  , examples (NumeralValue 12)
             [ "១២"
             , "ដប់ពីរ"
             ]
  , examples (NumeralValue 13)
             [ "១៣"
             , "ដប់បី"
             ]
  , examples (NumeralValue 14)
             [ "១៤"
             , "ដប់បួន"
             ]
  , examples (NumeralValue 15)
             [ "១៥"
             , "ដប់ប្រាំ"
             ]
  , examples (NumeralValue 16)
             [ "១៦"
             , "ដប់ប្រាំមួយ"
             ]
  , examples (NumeralValue 17)
             [ "១៧"
             , "ដប់ប្រាំពីរ"
             , "ដប់ប្រាំពិល"
             ]
  , examples (NumeralValue 18)
             [ "១៨"
             , "ដប់ប្រាំបី"
             ]
  , examples (NumeralValue 19)
             [ "១៩"
             , "ដប់ប្រាំបួន"
             ]
  , examples (NumeralValue 20)
             [ "២០"
             , "ម្ភៃ"
             ]
  , examples (NumeralValue 21)
             [ "២១"
             , "ម្ភៃមួយ"
             ]
  , examples (NumeralValue 22)
             [ "២២"
             , "ម្ភៃពីរ"
             ]
  , examples (NumeralValue 23)
             [ "២៣"
             , "ម្ភៃបី"
             ]
  , examples (NumeralValue 24)
             [ "២៤"
             , "ម្ភៃបួន"
             ]
  , examples (NumeralValue 25)
             [ "២៥"
             , "ម្ភៃប្រាំ"
             ]
  , examples (NumeralValue 26)
             [ "២៦"
             , "ម្ភៃប្រាំមួយ"
             ]
  , examples (NumeralValue 27)
             [ "២៧"
             , "ម្ភៃប្រាំពីរ"
             , "ម្ភៃប្រាំពិល"
             ]
  , examples (NumeralValue 28)
             [ "២៨"
             , "ម្ភៃប្រាំបី"
             ]
  , examples (NumeralValue 29)
             [ "២៩"
             , "ម្ភៃប្រាំបួន"
             ]                                       
  , examples (NumeralValue 30)
             [ "៣០"
             , "សាមសិប"
             ]
  , examples (NumeralValue 31)
             [ "៣១"
             , "សាមសិបមួយ"
             ]
  , examples (NumeralValue 40)
             [ "៤០"
             , "សែសិប"
             ]
  , examples (NumeralValue 41)
             [ "៤១"
             , "សែសិបមួយ"
             ]
  , examples (NumeralValue 50)
             [ "៥០"
             , "ហាសិប"
             ]
  , examples (NumeralValue 51)
             [ "៥១"
             , "ហាសិបមួយ"
             ]
  , examples (NumeralValue 60)
             [ "៦០"
             , "ហុកសិប"
             ]
  , examples (NumeralValue 61)
             [ "៦១"
             , "ហុកសិបមួយ"
             ]
  , examples (NumeralValue 70)
             [ "៧០"
             , "ចិតសិប"
             ]
  , examples (NumeralValue 71)
             [ "៧១"
             , "ចិតសិបមួយ"
             ]
  , examples (NumeralValue 80)
             [ "៨០"
             , "ប៉ែតសិប"
             ]
  , examples (NumeralValue 81)
             [ "៨១"
             , "ប៉ែតសិបមួយ"
             ]
  , examples (NumeralValue 90)
             [ "៩០"
             , "កៅសិប"
             ]
  , examples (NumeralValue 91)
             [ "៩១"
             , "កៅសិបមួយ"
             ]             
  , examples (NumeralValue 100)
             [ "១០០"
             , "មួយរយ"
             ]
  , examples (NumeralValue 101)
             [ "១០១"
             , "មួយរយមួយ"
             ]
  , examples (NumeralValue 110)
             [ "១១០"
             , "មួយរយដប់"
             ]
  , examples (NumeralValue 111)
             [ "១១១"
             , "មួយរយដប់មួយ"
             ]
  , examples (NumeralValue 200)
             [ "២០០"
             , "ពីររយ"
             ]
  , examples (NumeralValue 201)
             [ "២០១"
             , "ពីររយមួយ"
             ]
  ]
