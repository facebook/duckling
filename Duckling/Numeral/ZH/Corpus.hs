-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


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
  , examples (NumeralValue 203)
             [ "203"
             , "二百零三"
             ]
  , examples (NumeralValue 534)
             [ "534"
             , "五百三十四"
             ]
  , examples (NumeralValue 34567)
             [ "34567"
             , "34,567"
             , "三万四千五百六十七"
             ]
  , examples (NumeralValue 10040)
             [ "10040"
             , "10,040"
             , "一万零四十"
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
  , examples (NumeralValue 34507)
             [ "34507"
             , "34,507"
             , "三万四千五百零七"
             ]
  , examples (NumeralValue 100000)
             [ "100,000"
             , "100000"
             , "100K"
             , "100k"
             , "十万"
             ]
  , examples (NumeralValue 3000000)
             [ "3M"
             , "3000000"
             , "3,000,000"
             , "三百万"
             ]
  , examples (NumeralValue 1040000)
             [ "1,040,000"
             , "1040000"
             , "1.04M"
             , "一百零四万"
             ]
  , examples (NumeralValue 1200000)
             [ "1,200,000"
             , "1200000"
             , "1.2M"
             , ".0012G"
             , "一百二十万"
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
             , "负一百二十万"
             ]
  ]
