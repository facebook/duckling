-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Number.IT.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Lang
import Duckling.Number.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {lang = IT}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumberValue 0)
             [ "0"
             , "nulla"
             , "zero"
             ]
  , examples (NumberValue 1)
             [ "1"
             , "uno"
             , "Un"
             ]
  , examples (NumberValue 2)
             [ "2"
             , "due"
             ]
  , examples (NumberValue 3)
             [ "3"
             , "tre"
             ]
  , examples (NumberValue 4)
             [ "4"
             , "quattro"
             ]
  , examples (NumberValue 5)
             [ "5"
             , "cinque"
             ]
  , examples (NumberValue 6)
             [ "6"
             , "sei"
             ]
  , examples (NumberValue 7)
             [ "7"
             , "sette"
             ]
  , examples (NumberValue 8)
             [ "8"
             , "otto"
             ]
  , examples (NumberValue 9)
             [ "9"
             , "nove"
             ]
  , examples (NumberValue 10)
             [ "10"
             , "dieci"
             ]
  , examples (NumberValue 33)
             [ "33"
             , "trentatré"
             , "0033"
             ]
  , examples (NumberValue 11)
             [ "11"
             , "Undici"
             ]
  , examples (NumberValue 12)
             [ "12"
             , "dodici"
             ]
  , examples (NumberValue 13)
             [ "13"
             , "tredici"
             ]
  , examples (NumberValue 14)
             [ "14"
             , "quattordici"
             ]
  , examples (NumberValue 15)
             [ "15"
             , "quindici"
             ]
  , examples (NumberValue 16)
             [ "16"
             , "sedici"
             ]
  , examples (NumberValue 17)
             [ "17"
             , "diciassette"
             ]
  , examples (NumberValue 18)
             [ "18"
             , "diciotto"
             ]
  , examples (NumberValue 19)
             [ "19"
             , "diciannove"
             ]
  , examples (NumberValue 20)
             [ "20"
             , "venti"
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
             , "100K"
             , "100k"
             ]
  , examples (NumberValue 3000000)
             [ "3M"
             , "3000K"
             , "3000000"
             , "3.000.000"
             ]
  , examples (NumberValue 1200000)
             [ "1.200.000"
             , "1200000"
             , "1,2M"
             , "1200K"
             , ",0012G"
             ]
  , examples (NumberValue (-1200000))
             [ "- 1.200.000"
             , "-1200000"
             , "meno 1.200.000"
             , "negativo 1200000"
             , "-1,2M"
             , "-1200K"
             , "-,0012G"
             ]
  ]
