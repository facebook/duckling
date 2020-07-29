-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.MY.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale MY Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple 0)
             [ "၀"
             , "သုံည"
             , "မရှိ"
             ]
  , examples (simple 1)
             [ "၁"
             , "တစ်"
             , "ပထမ"
             ]
  , examples (simple 2)
             [ "၂"
             , "နှစ်"
             , "ဒုတိယ"
             ]
  , examples (simple 3)
             [ "၃"
             , "သုံး"
             , "တတိယ"
             , "3"
             ]
  , examples (simple 30)
             [ "သုံးဆယ်"
             ]
  , examples (simple 33)
             [ "သုံးဆယ့်သုံး"
             ]
  , examples (simple 14)
             [ "ဆယ့်လေး"
             ]
  , examples (simple 17)
             [ "ဆယ့်ခုနှစ်"
             ]
  , examples (simple 200)
             [ "နှစ်ရာ"
             ]
  , examples (simple 900)
             [ "ကိုးရာ"
             ]
  , examples (simple 5000)
             [ "ငါးထောင်"
             ]
  , examples (simple 80000)
             [ "ရှစ်သောင်း"
             ]
  ]
