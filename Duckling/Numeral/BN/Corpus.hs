-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.BN.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale BN Nothing}, testOptions,
          allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple 0)
            [ "শূন্য"
            , "০"
            ]
  , examples (simple 1)
            [ "এক"
            ]
  , examples (simple 2)
            [ "দুই"
            ]
  , examples (simple 3)
            [ "তিন"
            ]
  , examples (simple 4)
            [ "চার"
            ]
  , examples (simple 5)
            [ "পাঁচ"
            ]
  , examples (simple 6)
            [ "ছয়"
            ]
  , examples (simple 7)
            [ "সাত"
            ]
  , examples (simple 8)
            [ "আট"
            ]
  , examples (simple 9)
            [ "নয়"
            ]
  , examples (simple 10)
            [ "দশ"
            ]
  , examples (simple 11)
            [ "এগারো"
            ]
  , examples (simple 15)
            [ "পনেরো"
            ]
  , examples (simple 17)
            [ "সতেরো"
            ]
  , examples (simple 20)
            [ "কুড়ি"
            ]
  , examples (simple 22)
            [ "বাইশ"
            ]
  , examples (simple 24)
            [ "চব্বিশ"
            ]
  , examples (simple 25)
            [ "পঁচিশ"
            ]
  , examples (simple 26)
            [ "ছাব্বিশ"
            ]
  , examples (simple 28)
            [ "আঠাশ"
            ]
  , examples (simple 30)
            [ "তিরিশ"
            ]
  , examples (simple 40)
            [ "চল্লিশ"
            ]
  , examples (simple 50)
            [ "পঞ্চাশ"
            ]
  , examples (simple 70)
            [ "সত্তর"
            ]
  ]
