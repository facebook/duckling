-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Number.EN.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Number.Types
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumberValue 0)
             [ "0"
             , "naught"
             , "nought"
             , "zero"
             , "nil"
             ]
  , examples (NumberValue 1)
             [ "1"
             , "one"
             , "single"
             ]
  , examples (NumberValue 2)
             [ "2"
             , "two"
             , "a pair"
             , "a couple"
             , "a couple of"
             ]
  , examples (NumberValue 3)
             [ "3"
             , "three"
             , "a few"
             , "few"
             ]
  , examples (NumberValue 10)
             [ "10"
             , "ten"
             ]
  , examples (NumberValue 12)
             [ "12"
             , "twelve"
             , "a dozen"
             , "a dozen of"
             ]
  , examples (NumberValue 14)
             [ "14"
             , "fourteen"
             ]
  , examples (NumberValue 16)
             [ "16"
             , "sixteen"
             ]
  , examples (NumberValue 17)
             [ "17"
             , "seventeen"
             ]
  , examples (NumberValue 18)
             [ "18"
             , "eighteen"
             ]
  , examples (NumberValue 33)
             [ "33"
             , "thirty three"
             , "0033"
             ]
  , examples (NumberValue 24)
             [ "24"
             , "2 dozens"
             , "two dozen"
             , "Two dozen"
             ]
  , examples (NumberValue 1.1)
             [ "1.1"
             , "1.10"
             , "01.10"
             , "1 point 1"
             ]
  , examples (NumberValue 0.77)
             [ ".77"
             , "0.77"
             , "point 77"
             ]
  , examples (NumberValue 100000)
             [ "100,000"
             , "100,000.0"
             , "100000"
             , "100K"
             , "100k"
             , "one hundred thousand"
             ]
  , examples (NumberValue 3e6)
             [ "3M"
             , "3000K"
             , "3000000"
             , "3,000,000"
             , "3 million"
             ]
  , examples (NumberValue 1.2e6)
             [ "1,200,000"
             , "1200000"
             , "1.2M"
             , "1200k"
             , ".0012G"
             ]
  , examples (NumberValue 5000)
             [ "5 thousand"
             , "five thousand"
             ]
  , examples (NumberValue (-1.2e6))
             [ "- 1,200,000"
             , "-1200000"
             , "minus 1,200,000"
             , "negative 1200000"
             , "-1.2M"
             , "-1200K"
             , "-.0012G"
             ]
  , examples (NumberValue 122)
             [ "one twenty two"
             , "ONE TwentY tWO"
             ]
  , examples (NumberValue 2e5)
             [ "two Hundred thousand"
             ]
  , examples (NumberValue 21011)
             [ "twenty-one thousand Eleven"
             ]
  , examples (NumberValue 721012)
             [ "seven hundred twenty-one thousand twelve"
             , "seven hundred twenty-one thousand and twelve"
             ]
  , examples (NumberValue 31256721)
             [ "thirty-one million two hundred fifty-six thousand seven hundred twenty-one"
             ]
  , examples (NumberValue 2400)
             [ "two hundred dozens"
             , "200 dozens"
             ]
  , examples (NumberValue 2200000)
             [ "two point two million"
             ]
  ]
