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
  [ examples (NumeralValue 0)
             [ "၀"
             , "သုံည"
             , "မရှိ"
             ]
  , examples (NumeralValue 1)
             [ "၁"
             , "တစ်"
             , "ပထမ"
             ]
  , examples (NumeralValue 2)
             [ "၂"
             , "နှစ်"
             , "ဒုတိယ"
             ]
  , examples (NumeralValue 3)
             [ "၃"
             , "သုံး"
             , "တတိယ"
             , "3"
             ]
  , examples (NumeralValue 30)
             [ "သုံးဆယ်"
             ]
  , examples (NumeralValue 33)
             [ "သုံးဆယ့်သုံး"
             ]
  , examples (NumeralValue 14)
             [ "ဆယ့်လေး"
             ]
  , examples (NumeralValue 17)
             [ "ဆယ့်ခုနှစ်"
             ]
  , examples (NumeralValue 200)
             [ "နှစ်ရာ"
             ]
  , examples (NumeralValue 900)
             [ "ကိုးရာ"
             ]
  , examples (NumeralValue 5000)
             [ "ငါးထောင်"
             ]
  , examples (NumeralValue 80000)
             [ "ရှစ်သောင်း"
             ]
  ]
