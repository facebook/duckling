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
  [ examples (simple 0)
             [ "0"
             , "០"
             , "សូន្យ"
             ]
  , examples (simple 1)
             [ "1"
             , "១"
             , "មួយ"
             ]
  , examples (simple 2)
             [ "2"
             , "២"
             , "ពីរ"
             ]
  , examples (simple 3)
             [ "3"
             , "៣"
             , "បី"
             ]
  , examples (simple 4)
             [ "4"
             , "៤"
             , "បួន"
             ]
  , examples (simple 5)
             [ "5"
             , "៥"
             , "ប្រាំ"
             ]
  , examples (simple 6)
             [ "6"
             , "៦"
             , "ប្រាំមួយ"
             ]
  , examples (simple 7)
             [ "7"
             , "៧"
             , "ប្រាំពីរ"
             ]
  , examples (simple 8)
             [ "8"
             , "៨"
             , "ប្រាំបី"
             ]
  , examples (simple 9)
             [ "9"
             , "៩"
             , "ប្រាំបួន"
             ]
  , examples (simple 10)
             [ "ដប់"
             ]
  , examples (simple 11)
             [ "ដប់មួយ"
             ]
  , examples (simple 22)
             [ "ម្ភៃពីរ"
             ]
  , examples (simple 33)
             [ "សាមបី"
             ]
  , examples (simple 99)
             [ "កៅប្រាំបួន"
             ]
  , examples (simple 320)
             [ "បីរយម្ភៃ"
             ]
  , examples (simple 6078)
             [ "ប្រាំមួយពាន់ចិតប្រាំបី"
             ]
  , examples (simple 5689443)
             [ "ប្រាំលានប្រាំមួយសែនប្រាំបីម៉ឺនប្រាំបួនពាន់បួនរយសែបី"
             ]
  , examples (simple 800000000)
             [ "ប្រាំបីរយលាន"
             ]
  ]
