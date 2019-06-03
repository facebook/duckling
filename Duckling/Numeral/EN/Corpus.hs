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
  [ examples (NumeralValue 0)
             [ "0"
             , "naught"
             , "nought"
             , "zero"
             , "nil"
             ]
  , examples (NumeralValue 1)
             [ "1"
             , "one"
             , "single"
             ]
  , examples (NumeralValue 2)
             [ "2"
             , "two"
             , "a pair"
             , "a couple"
             , "a couple of"
             ]
  , examples (NumeralValue 3)
             [ "3"
             , "three"
             , "a few"
             , "few"
             ]
  , examples (NumeralValue 10)
             [ "10"
             , "ten"
             ]
  , examples (NumeralValue 12)
             [ "12"
             , "twelve"
             , "a dozen"
             , "a dozen of"
             ]
  , examples (NumeralValue 14)
             [ "14"
             , "fourteen"
             ]
  , examples (NumeralValue 16)
             [ "16"
             , "sixteen"
             ]
  , examples (NumeralValue 17)
             [ "17"
             , "seventeen"
             ]
  , examples (NumeralValue 18)
             [ "18"
             , "eighteen"
             ]
  , examples (NumeralValue 33)
             [ "33"
             , "thirty three"
             , "0033"
             ]
  , examples (NumeralValue 24)
             [ "24"
             , "2 dozens"
             , "two dozen"
             , "Two dozen"
             ]
  , examples (NumeralValue 1.1)
             [ "1.1"
             , "1.10"
             , "01.10"
             , "1 point 1"
             ]
  , examples (NumeralValue 0.77)
             [ ".77"
             , "0.77"
             , "point 77"
             ]
  , examples (NumeralValue 100000)
             [ "100,000"
             , "100,000.0"
             , "100000"
             , "100K"
             , "100k"
             , "one hundred thousand"
             ]
  , examples (NumeralValue 0.2)
             [ "1/5"
             , "2/10"
             , "3/15"
             , "20/100"
             ]
  , examples (NumeralValue 3e6)
             [ "3M"
             , "3000K"
             , "3000000"
             , "3,000,000"
             , "3 million"
             , "30 lakh"
             ]
  , examples (NumeralValue 1.2e6)
             [ "1,200,000"
             , "1200000"
             , "1.2M"
             , "1200k"
             , ".0012G"
             , "12 lakhs"
             ]
  , examples (NumeralValue 5000)
             [ "5 thousand"
             , "five thousand"
             ]
  , examples (NumeralValue (-504))
             [ "-504"
             , "negative five hundred and four"
             ]
  , examples (NumeralValue (-1.2e6))
             [ "- 1,200,000"
             , "-1200000"
             , "minus 1,200,000"
             , "negative 1200000"
             , "-1.2M"
             , "-1200K"
             , "-.0012G"
             ]
  , examples (NumeralValue 122)
             [ "one twenty two"
             , "ONE TwentY tWO"
             ]
  , examples (NumeralValue 2e5)
             [ "two Hundred thousand"
             ]
  , examples (NumeralValue 21011)
             [ "twenty-one thousand Eleven"
             ]
  , examples (NumeralValue 721012)
             [ "seven hundred twenty-one thousand twelve"
             , "seven hundred twenty-one thousand and twelve"
             ]
  , examples (NumeralValue 31256721)
             [ "thirty-one million two hundred fifty-six thousand seven hundred twenty-one"
             , "three crore twelve lakh fifty-six thousand seven hundred twenty-one"
             ]
  , examples (NumeralValue 2400)
             [ "two hundred dozens"
             , "200 dozens"
             ]
  , examples (NumeralValue 2200000)
             [ "two point two million"
             ]
  , examples (NumeralValue 3000000000)
             [ "three billions"
             , "three thousand millions"
             , "three hundred crores"
             ]
  ]
