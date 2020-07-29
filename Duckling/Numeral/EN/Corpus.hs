-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.EN.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Numeral.Types
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple 0)
             [ "0"
             , "naught"
             , "nought"
             , "zero"
             , "nil"
             ]
  , examples (simple 1)
             [ "1"
             , "one"
             , "single"
             ]
  , examples (simple 2)
             [ "2"
             , "two"
             , "a pair"
             , "a couple"
             , "a couple of"
             ]
  , examples (simple 3)
             [ "3"
             , "three"
             , "a few"
             , "few"
             ]
  , examples (simple 10)
             [ "10"
             , "ten"
             ]
  , examples (simple 12)
             [ "12"
             , "twelve"
             , "a dozen"
             , "a dozen of"
             ]
  , examples (simple 14)
             [ "14"
             , "fourteen"
             ]
  , examples (simple 16)
             [ "16"
             , "sixteen"
             ]
  , examples (simple 17)
             [ "17"
             , "seventeen"
             ]
  , examples (simple 18)
             [ "18"
             , "eighteen"
             ]
  , examples (simple 33)
             [ "33"
             , "thirty three"
             , "0033"
             ]
  , examples (simple 24)
             [ "24"
             , "2 dozens"
             , "two dozen"
             , "Two dozen"
             ]
  , examples (simple 1.1)
             [ "1.1"
             , "1.10"
             , "01.10"
             , "1 point 1"
             ]
  , examples (simple 0.77)
             [ ".77"
             , "0.77"
             , "point 77"
             ]
  , examples (simple 100000)
             [ "100,000"
             , "100,000.0"
             , "100000"
             , "100K"
             , "100k"
             , "one hundred thousand"
             ]
  , examples (simple 0.2)
             [ "1/5"
             , "2/10"
             , "3/15"
             , "20/100"
             ]
  , examples (simple 3e6)
             [ "3M"
             , "3000K"
             , "3000000"
             , "3,000,000"
             , "3 million"
             , "30 lakh"
             ]
  , examples (simple 1.2e6)
             [ "1,200,000"
             , "1200000"
             , "1.2M"
             , "1200k"
             , ".0012G"
             , "12 lakhs"
             ]
  , examples (simple 5000)
             [ "5 thousand"
             , "five thousand"
             ]
  , examples (simple (-504))
             [ "-504"
             , "negative five hundred and four"
             ]
  , examples (simple (-1.2e6))
             [ "- 1,200,000"
             , "-1200000"
             , "minus 1,200,000"
             , "negative 1200000"
             , "-1.2M"
             , "-1200K"
             , "-.0012G"
             ]
  , examples (simple (-3200000))
             [ "-3,200,000"
             , "-3200000"
             , "minus three million two hundred thousand"
             ]
  , examples (simple 122)
             [ "one twenty two"
             , "ONE TwentY tWO"
             ]
  , examples (simple 2e5)
             [ "two Hundred thousand"
             ]
  , examples (simple 21011)
             [ "twenty-one thousand Eleven"
             ]
  , examples (simple 721012)
             [ "seven hundred twenty-one thousand twelve"
             , "seven hundred twenty-one thousand and twelve"
             ]
  , examples (simple 31256721)
             [ "thirty-one million two hundred fifty-six thousand seven hundred twenty-one"
             , "three crore twelve lakh fifty-six thousand seven hundred twenty-one"
             ]
  , examples (simple 2400)
             [ "two hundred dozens"
             , "200 dozens"
             ]
  , examples (simple 2200000)
             [ "two point two million"
             ]
  , examples (simple 3000000000)
             [ "three billions"
             , "three thousand millions"
             , "three hundred crores"
             ]
  , examples (between (600,900))
             [ "between 600 and 900"
             , "from six hundred to nine hundred"
             , "around 600-900"
             , "about 600-900"
             , "600-900"
             ]
  , examples (under 0.5)
             [ "under 0.5"
             , "less than 0.5"
             , "lower than point five"
             ]
  , examples (above 3000)
             [ "more than three thousand"
             , "at least 3000"
             , "over 3K"
             , "above 3 thousand"
             ]
  ]
