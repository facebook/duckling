-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.NB.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale NB Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumeralValue 0)
             [ "0"
             , "null"
             ]
  , examples (NumeralValue 1)
             [ "1"
             , "én"
             , "en"
             , "Ett"
             ]
  , examples (NumeralValue 2)
             [ "2"
             , "to"
             , "et par"
             ]
  , examples (NumeralValue 7)
             [ "7"
             , "syv"
             , "sju"
             ]
  , examples (NumeralValue 14)
             [ "14"
             , "fjorten"
             ]
  , examples (NumeralValue 16)
             [ "16"
             , "seksten"
             ]
  , examples (NumeralValue 17)
             [ "17"
             , "sytten"
             , "søtten"
             ]
  , examples (NumeralValue 18)
             [ "18"
             , "atten"
             ]
  , examples (NumeralValue 20)
             [ "20"
             , "tyve"
             , "Tjue"
             ]
  , examples (NumeralValue 30)
             [ "30"
             , "tretti"
             , "tredve"
             ]
  , examples (NumeralValue 70)
             [ "70"
             , "søtti"
             , "sytti"
             ]
  , examples (NumeralValue 1.1)
             [ "1,1"
             , "1,10"
             , "01,10"
             ]
  , examples (NumeralValue 0.77)
             [ "0,77"
             , ",77"
             ]
  , examples (NumeralValue 100000)
             [ "100.000"
             , "100000"
             , "100K"
             , "100k"
             ]
  , examples (NumeralValue 3000000)
             [ "3M"
             , "3000K"
             , "3000000"
             , "3.000.000"
             ]
  , examples (NumeralValue 1200000)
             [ "1.200.000"
             , "1200000"
             , "1,2M"
             , "1200K"
             , ",0012G"
             ]
  , examples (NumeralValue (-1200000))
             [ "- 1.200.000"
             , "-1200000"
             , "minus 1.200.000"
             , "negativ 1200000"
             , "-1,2M"
             , "-1200K"
             , "-,0012G"
             ]
  , examples (NumeralValue 5000)
             [ "5 tusen"
             , "fem tusen"
             ]
  , examples (NumeralValue 100)
             [ "hundre"
             ]
  , examples (NumeralValue 5020)
             [ "fem tusen og tjue"
             ]
  ]
