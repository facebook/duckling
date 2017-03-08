-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Number.MY.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Lang
import Duckling.Number.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {lang = MY}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumberValue 0)
             [ "၀"
             , "သုံည"
             , "မရှိ"
             ]
  , examples (NumberValue 1)
             [ "၁"
             , "တစ်"
             , "ပထမ"
             ]
  , examples (NumberValue 2)
             [ "၂"
             , "နှစ်"
             , "ဒုတိယ"
             ]
  , examples (NumberValue 3)
             [ "၃"
             , "သုံး"
             , "တတိယ"
             ]
  , examples (NumberValue 30)
             [ "သုံးဆယ်"
             ]
  , examples (NumberValue 33)
             [ "သုံးဆယ့်သုံး"
             ]
  , examples (NumberValue 14)
             [ "ဆယ့်လေး"
             ]
  , examples (NumberValue 17)
             [ "ဆယ့်ခုနှစ်"
             ]
  , examples (NumberValue 200)
             [ "နှစ်ရာ"
             ]
  , examples (NumberValue 900)
             [ "ကိုးရာ"
             ]
  , examples (NumberValue 5000)
             [ "ငါးထောင်"
             ]
  , examples (NumberValue 80000)
             [ "ရှစ်သောင်း"
             ]
  ]
