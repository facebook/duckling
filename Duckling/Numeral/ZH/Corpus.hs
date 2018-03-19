-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.ZH.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale ZH Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumeralValue 0)
             [ "0"
             , "〇"
             , "零"
             , "零个"
             , "0个"
             ]
  , examples (NumeralValue 1)
             [ "1"
             , "一"
             , "一个"
             , "1个"
             ]
  , examples (NumeralValue 2)
             [ "2"
             , "二個"
             , "二个"
             ]
  , examples (NumeralValue 10)
             [ "10"
             , "十"
             ]
  , examples (NumeralValue 11)
             [ "11"
             , "十一"
             ]
  , examples (NumeralValue 20)
             [ "20"
             , "二十"
             ]
  , examples (NumeralValue 60)
             [ "60"
             , "六十"
             ]
  , examples (NumeralValue 33)
             [ "33"
             , "三十三"
             ]
  , examples (NumeralValue 96)
             [ "96"
             , "九十六"
             ]
  , examples (NumeralValue 1.1)
             [ "1.1"
             , "1.10"
             , "01.10"
             ]
  , examples (NumeralValue 0.77)
             [ "0.77"
             , ".77"
             ]
  , examples (NumeralValue 100000)
             [ "100,000"
             , "100000"
             , "100K"
             , "100k"
             ]
  , examples (NumeralValue 3000000)
             [ "3M"
             , "3000K"
             , "3000000"
             , "3,000,000"
             ]
  , examples (NumeralValue 1200000)
             [ "1,200,000"
             , "1200000"
             , "1.2M"
             , "1200K"
             , ".0012G"
             ]
  , examples (NumeralValue (-1200000))
             [ "- 1,200,000"
             , "-1200000"
             , "负1,200,000"
             , "负 1,200,000"
             , "負 1,200,000"
             , "负1200000"
             , "负 1200000"
             , "-1.2M"
             , "-1200K"
             , "-.0012G"
             ]
  ]
