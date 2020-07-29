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
  [ examples (simple 0)
             [ "0"
             , "zero"
             , "nici unul"
             , "nici unu"
             , "nici una"
             , "nici o"
             , "nicio"
             , "nimic"
             ]
  , examples (simple 1)
             [ "1"
             , "unu"
             , "unul"
             , "un"
             , "o"
             , "intai"
             , "întâi"
             ]
  , examples (simple 2)
             [ "2"
             , "doi"
             , "doua"
             , "două"
             ]
  , examples (simple 3)
             [ "3"
             , "trei"
             ]
  , examples (simple 4)
             [ "4"
             , "patru"
             ]
  , examples (simple 5)
             [ "5"
             , "cinci"
             ]
  , examples (simple 6)
             [ "6"
             , "sase"
             , "șase"
             ]
  , examples (simple 7)
             [ "7"
             , "sapte"
             , "șapte"
             ]
  , examples (simple 8)
             [ "8"
             , "opt"
             ]
  , examples (simple 9)
             [ "9"
             , "noua"
             , "nouă"
             ]
  , examples (simple 10)
             [ "10"
             , "zece"
             , "zeci"
             ]
  , examples (simple 10)
             [ "10"
             , "zece"
             ]
  , examples (simple 11)
             [ "11"
             , "unsprezece"
             , "unspe"
             , "unșpe"
             ]
  , examples (simple 19)
             [ "19"
             , "nouasprezece"
             , "nouaspe"
             , "nouăsprezece"
             , "nouășpe"
             ]
  , examples (simple 70)
             [ "70"
             , "sapte zeci"
             , "șapte zeci"
             ]
  , examples (simple 1.1)
             [ "1,1"
             , "1,10"
             , "01,10"
             ]
  , examples (simple 0.77)
             [ "0,77"
             , ",77"
             ]
  , examples (simple 100000)
             [ "100.000"
             , "100000"
             ]
  , examples (simple 3000000)
             [ "3000000"
             , "3.000.000"
             ]
  , examples (simple 1200000)
             [ "1.200.000"
             , "1200000"
             ]
  , examples (simple (-1200000))
             [ "-1.200.000"
             , "-1200000"
             , "minus 1.200.000"
             ]
  , examples (simple (-3))
             [ "-3"
             , "3 negativ"
             ]
  , examples (simple 5000)
             [ "5 mii"
             , "cinci mii"
             ]
  , examples (simple 1000)
             [ "o mie"
             , "1 mie"
             ]
  , examples (simple 100)
             [ "o suta"
             , "o sută"
             , "1 suta"
             , "1 sută"
             ]
  , examples (simple 300)
             [ "3 sute"
             , "trei sute"
             ]
  , examples (simple 1000000)
             [ "un milion"
             , "1 milion"
             ]
  , examples (simple 7000000)
             [ "7 milioane"
             , "sapte milioane"
             , "șapte milioane"
             ]
  , examples (simple 21000000)
             [ "21 de milioane"
             , "douazeci si unu de milioane"
             ]
  , examples (simple 1000000000)
             [ "un miliard"
             , "1 miliard"
             ]
  , examples (simple 9000000000)
             [ "9 miliarde"
             , "noua miliarde"
             , "nouă miliarde"
             ]
  , examples (simple 20)
             [ "20"
             , "douazeci"
             , "doua zeci"
             , "douăzeci"
             , "două zeci"
             ]
  , examples (simple 23)
             [ "23"
             , "20 3"
             , "douazeci 3"
             , "douăzeci 3"
             , "douăzeci trei"
             , "douazeci si trei"
             , "douăzeci și trei"
             ]
  ]
