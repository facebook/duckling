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
  [ examples (NumeralValue 0)
            [ "শূন্য"
            , "০"
            ]
  , examples (NumeralValue 1)
            [ "এক"
            ]
  , examples (NumeralValue 2)
            [ "দুই"
            ]
  , examples (NumeralValue 3)
            [ "তিন"
            ]
  , examples (NumeralValue 4)
            [ "চার"
            ]
  , examples (NumeralValue 5)
            [ "পাঁচ"
            ]
  , examples (NumeralValue 6)
            [ "ছয়"
            ]
  , examples (NumeralValue 7)
            [ "সাত"
            ]
  , examples (NumeralValue 8)
            [ "আট"
            ]
  , examples (NumeralValue 9)
            [ "নয়"
            ]
  , examples (NumeralValue 10)
            [ "দশ"
            ]
  , examples (NumeralValue 11)
            [ "এগারো"
            ]
  , examples (NumeralValue 15)
            [ "পনেরো"
            ]
  , examples (NumeralValue 17)
            [ "সতেরো"
            ]
  , examples (NumeralValue 20)
            [ "কুড়ি"
            ]
  , examples (NumeralValue 22)
            [ "বাইশ"
            ]
  , examples (NumeralValue 24)
            [ "চব্বিশ"
            ]
  , examples (NumeralValue 25)
            [ "পঁচিশ"
            ]
  , examples (NumeralValue 26)
            [ "ছাব্বিশ"
            ]
  , examples (NumeralValue 28)
            [ "আঠাশ"
            ]
  , examples (NumeralValue 30)
            [ "তিরিশ"
            ]
  , examples (NumeralValue 40)
            [ "চল্লিশ"
            ]
  , examples (NumeralValue 50)
            [ "পঞ্চাশ"
            ]
  , examples (NumeralValue 70)
            [ "সত্তর"
            ]
  ]
