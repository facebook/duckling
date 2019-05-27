-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.RO.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

context :: Context
context = testContext {locale = makeLocale RO Nothing}

corpus :: Corpus
corpus = (context, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumeralValue 0)
             [ "0"
             , "zero"
             , "nici unul"
             , "nici unu"
             , "nici una"
             , "nici o"
             , "nicio"
             , "nimic"
             ]
  , examples (NumeralValue 1)
             [ "1"
             , "unu"
             , "unul"
             , "un"
             , "o"
             , "intai"
             , "întâi"
             ]
  , examples (NumeralValue 2)
             [ "2"
             , "doi"
             , "doua"
             , "două"
             ]
  , examples (NumeralValue 3)
             [ "3"
             , "trei"
             ]
  , examples (NumeralValue 4)
             [ "4"
             , "patru"
             ]
  , examples (NumeralValue 5)
             [ "5"
             , "cinci"
             ]
  , examples (NumeralValue 6)
             [ "6"
             , "sase"
             , "șase"
             ]
  , examples (NumeralValue 7)
             [ "7"
             , "sapte"
             , "șapte"
             ]
  , examples (NumeralValue 8)
             [ "8"
             , "opt"
             ]
  , examples (NumeralValue 9)
             [ "9"
             , "noua"
             , "nouă"
             ]
  , examples (NumeralValue 10)
             [ "10"
             , "zece"
             , "zeci"
             ]
  , examples (NumeralValue 10)
             [ "10"
             , "zece"
             ]
  , examples (NumeralValue 11)
             [ "11"
             , "unsprezece"
             , "unspe"
             , "unșpe"
             ]
  , examples (NumeralValue 19)
             [ "19"
             , "nouasprezece"
             , "nouaspe"
             , "nouăsprezece"
             , "nouășpe"
             ]
  , examples (NumeralValue 70)
             [ "70"
             , "sapte zeci"
             , "șapte zeci"
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
             ]
  , examples (NumeralValue 3000000)
             [ "3000000"
             , "3.000.000"
             ]
  , examples (NumeralValue 1200000)
             [ "1.200.000"
             , "1200000"
             ]
  , examples (NumeralValue (-1200000))
             [ "-1.200.000"
             , "-1200000"
             , "minus 1.200.000"
             ]
  , examples (NumeralValue (-3))
             [ "-3"
             , "3 negativ"
             ]
  , examples (NumeralValue 5000)
             [ "5 mii"
             , "cinci mii"
             ]
  , examples (NumeralValue 1000)
             [ "o mie"
             , "1 mie"
             ]
  , examples (NumeralValue 100)
             [ "o suta"
             , "o sută"
             , "1 suta"
             , "1 sută"
             ]
  , examples (NumeralValue 300)
             [ "3 sute"
             , "trei sute"
             ]
  , examples (NumeralValue 1000000)
             [ "un milion"
             , "1 milion"
             ]
  , examples (NumeralValue 7000000)
             [ "7 milioane"
             , "sapte milioane"
             , "șapte milioane"
             ]
  , examples (NumeralValue 21000000)
             [ "21 de milioane"
             , "douazeci si unu de milioane"
             ]
  , examples (NumeralValue 1000000000)
             [ "un miliard"
             , "1 miliard"
             ]
  , examples (NumeralValue 9000000000)
             [ "9 miliarde"
             , "noua miliarde"
             , "nouă miliarde"
             ]
  , examples (NumeralValue 20)
             [ "20"
             , "douazeci"
             , "doua zeci"
             , "douăzeci"
             , "două zeci"
             ]
  , examples (NumeralValue 23)
             [ "23"
             , "20 3"
             , "douazeci 3"
             , "douăzeci 3"
             , "douăzeci trei"
             , "douazeci si trei"
             , "douăzeci și trei"
             ]
  ]
