-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Number.RO.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Lang
import Duckling.Number.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {lang = RO}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumberValue 0)
             [ "0"
             , "zero"
             , "nici unul"
             , "nici unu"
             , "nici una"
             , "nici o"
             , "nicio"
             , "nimic"
             ]
  , examples (NumberValue 1)
             [ "1"
             , "unu"
             , "unul"
             , "un"
             , "o"
             , "intai"
             , "întâi"
             ]
  , examples (NumberValue 2)
             [ "2"
             , "doi"
             , "doua"
             , "două"
             ]
  , examples (NumberValue 3)
             [ "3"
             , "trei"
             ]
  , examples (NumberValue 4)
             [ "4"
             , "patru"
             ]
  , examples (NumberValue 5)
             [ "5"
             , "cinci"
             ]
  , examples (NumberValue 6)
             [ "6"
             , "sase"
             , "șase"
             ]
  , examples (NumberValue 7)
             [ "7"
             , "sapte"
             , "șapte"
             ]
  , examples (NumberValue 8)
             [ "8"
             , "opt"
             ]
  , examples (NumberValue 9)
             [ "9"
             , "noua"
             , "nouă"
             ]
  , examples (NumberValue 10)
             [ "10"
             , "zece"
             , "zeci"
             ]
  , examples (NumberValue 10)
             [ "10"
             , "zece"
             ]
  , examples (NumberValue 11)
             [ "11"
             , "unsprezece"
             , "unspe"
             , "unșpe"
             ]
  , examples (NumberValue 19)
             [ "19"
             , "nouasprezece"
             , "nouaspe"
             , "nouăsprezece"
             , "nouășpe"
             ]
  , examples (NumberValue 70)
             [ "70"
             , "sapte zeci"
             , "șapte zeci"
             ]
  , examples (NumberValue 1.1)
             [ "1,1"
             , "1,10"
             , "01,10"
             ]
  , examples (NumberValue 0.77)
             [ "0,77"
             , ",77"
             ]
  , examples (NumberValue 100000)
             [ "100.000"
             , "100000"
             ]
  , examples (NumberValue 3000000)
             [ "3000000"
             , "3.000.000"
             ]
  , examples (NumberValue 1200000)
             [ "1.200.000"
             , "1200000"
             ]
  , examples (NumberValue (-1200000))
             [ "-1.200.000"
             , "-1200000"
             , "minus 1.200.000"
             ]
  , examples (NumberValue (-3))
             [ "-3"
             , "3 negativ"
             ]
  , examples (NumberValue 5000)
             [ "5 mii"
             , "cinci mii"
             ]
  , examples (NumberValue 1000)
             [ "o mie"
             , "1 mie"
             ]
  , examples (NumberValue 100)
             [ "o suta"
             , "o sută"
             , "1 suta"
             , "1 sută"
             ]
  , examples (NumberValue 300)
             [ "3 sute"
             , "trei sute"
             ]
  , examples (NumberValue 1000000)
             [ "un milion"
             , "1 milion"
             ]
  , examples (NumberValue 7000000)
             [ "7 milioane"
             , "sapte milioane"
             , "șapte milioane"
             ]
  , examples (NumberValue 1000000000)
             [ "un miliard"
             , "1 miliard"
             ]
  , examples (NumberValue 9000000000)
             [ "9 miliarde"
             , "noua miliarde"
             , "nouă miliarde"
             ]
  , examples (NumberValue 20)
             [ "20"
             , "douazeci"
             , "doua zeci"
             , "douăzeci"
             , "două zeci"
             ]
  , examples (NumberValue 23)
             [ "23"
             , "20 3"
             , "douazeci 3"
             , "douăzeci 3"
             , "douăzeci trei"
             , "douazeci si trei"
             , "douăzeci și trei"
             ]
  ]
