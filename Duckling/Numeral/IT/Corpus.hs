-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.IT.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale IT Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple 0)
             [ "0"
             , "nulla"
             , "zero"
             ]
  , examples (simple 1)
             [ "1"
             , "uno"
             , "Un"
             ]
  , examples (simple 2)
             [ "2"
             , "due"
             ]
  , examples (simple 3)
             [ "3"
             , "tre"
             ]
  , examples (simple 4)
             [ "4"
             , "quattro"
             ]
  , examples (simple 5)
             [ "5"
             , "cinque"
             ]
  , examples (simple 6)
             [ "6"
             , "sei"
             ]
  , examples (simple 7)
             [ "7"
             , "sette"
             ]
  , examples (simple 8)
             [ "8"
             , "otto"
             ]
  , examples (simple 9)
             [ "9"
             , "nove"
             ]
  , examples (simple 10)
             [ "10"
             , "dieci"
             ]
  , examples (simple 33)
             [ "33"
             , "trentatré"
             , "0033"
             ]
  , examples (simple 11)
             [ "11"
             , "Undici"
             ]
  , examples (simple 12)
             [ "12"
             , "dodici"
             ]
  , examples (simple 13)
             [ "13"
             , "tredici"
             ]
  , examples (simple 14)
             [ "14"
             , "quattordici"
             ]
  , examples (simple 15)
             [ "15"
             , "quindici"
             ]
  , examples (simple 16)
             [ "16"
             , "sedici"
             ]
  , examples (simple 17)
             [ "17"
             , "diciassette"
             ]
  , examples (simple 18)
             [ "18"
             , "diciotto"
             ]
  , examples (simple 19)
             [ "19"
             , "diciannove"
             ]
  , examples (simple 20)
             [ "20"
             , "venti"
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
             , "100K"
             , "100k"
             , "100 000"
             ]
  , examples (simple 3000000)
             [ "3M"
             , "3000K"
             , "3000000"
             , "3.000.000"
             , "3 000 000"
             ]
  , examples (simple 1200000)
             [ "1.200.000"
             , "1200000"
             , "1,2M"
             , "1200K"
             , ",0012G"
             ]
  , examples (simple (-1200000))
             [ "- 1.200.000"
             , "-1200000"
             , "meno 1.200.000"
             , "negativo 1200000"
             , "-1,2M"
             , "-1200K"
             , "-,0012G"
             ]
  , examples (simple 6.7)
             [ "6,7"
             ]
  , examples (simple 6700.54)
             [ "6.700,54"
             , "6 700,54"
             ]
  ]
