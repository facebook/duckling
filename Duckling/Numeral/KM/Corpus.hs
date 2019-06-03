-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.KM.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

context :: Context
context = testContext {locale = makeLocale KM Nothing}

corpus :: Corpus
corpus = (context, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumeralValue 0)
             [ "0"
             , "០"
             , "សូន្យ"
             ]
  , examples (NumeralValue 1)
             [ "1"
             , "១"
             , "មួយ"
             ]
  , examples (NumeralValue 2)
             [ "2"
             , "២"
             , "ពីរ"
             ]
  , examples (NumeralValue 3)
             [ "3"
             , "៣"
             , "បី"
             ]
  , examples (NumeralValue 4)
             [ "4"
             , "៤"
             , "បួន"
             ]
  , examples (NumeralValue 5)
             [ "5"
             , "៥"
             , "ប្រាំ"
             ]
  , examples (NumeralValue 6)
             [ "6"
             , "៦"
             , "ប្រាំមួយ"
             ]
  , examples (NumeralValue 7)
             [ "7"
             , "៧"
             , "ប្រាំពីរ"
             ]
  , examples (NumeralValue 8)
             [ "8"
             , "៨"
             , "ប្រាំបី"
             ]
  , examples (NumeralValue 9)
             [ "9"
             , "៩"
             , "ប្រាំបួន"
             ]
  , examples (NumeralValue 10)
             [ "ដប់"
             ]
  , examples (NumeralValue 11)
             [ "ដប់មួយ"
             ]
  , examples (NumeralValue 22)
             [ "ម្ភៃពីរ"
             ]
  , examples (NumeralValue 33)
             [ "សាមបី"
             ]
  , examples (NumeralValue 99)
             [ "កៅប្រាំបួន"
             ]
  , examples (NumeralValue 320)
             [ "បីរយម្ភៃ"
             ]
  , examples (NumeralValue 6078)
             [ "ប្រាំមួយពាន់ចិតប្រាំបី"
             ]
  , examples (NumeralValue 5689443)
             [ "ប្រាំលានប្រាំមួយសែនប្រាំបីម៉ឺនប្រាំបួនពាន់បួនរយសែបី"
             ]
  , examples (NumeralValue 800000000)
             [ "ប្រាំបីរយលាន"
             ]
  ]
