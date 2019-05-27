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
  [ examples (NumeralValue 0)
             [ "0"
             , "nulla"
             , "zero"
             ]
  , examples (NumeralValue 1)
             [ "1"
             , "uno"
             , "Un"
             ]
  , examples (NumeralValue 2)
             [ "2"
             , "due"
             ]
  , examples (NumeralValue 3)
             [ "3"
             , "tre"
             ]
  , examples (NumeralValue 4)
             [ "4"
             , "quattro"
             ]
  , examples (NumeralValue 5)
             [ "5"
             , "cinque"
             ]
  , examples (NumeralValue 6)
             [ "6"
             , "sei"
             ]
  , examples (NumeralValue 7)
             [ "7"
             , "sette"
             ]
  , examples (NumeralValue 8)
             [ "8"
             , "otto"
             ]
  , examples (NumeralValue 9)
             [ "9"
             , "nove"
             ]
  , examples (NumeralValue 10)
             [ "10"
             , "dieci"
             ]
  , examples (NumeralValue 33)
             [ "33"
             , "trentatré"
             , "0033"
             ]
  , examples (NumeralValue 11)
             [ "11"
             , "Undici"
             ]
  , examples (NumeralValue 12)
             [ "12"
             , "dodici"
             ]
  , examples (NumeralValue 13)
             [ "13"
             , "tredici"
             ]
  , examples (NumeralValue 14)
             [ "14"
             , "quattordici"
             ]
  , examples (NumeralValue 15)
             [ "15"
             , "quindici"
             ]
  , examples (NumeralValue 16)
             [ "16"
             , "sedici"
             ]
  , examples (NumeralValue 17)
             [ "17"
             , "diciassette"
             ]
  , examples (NumeralValue 18)
             [ "18"
             , "diciotto"
             ]
  , examples (NumeralValue 19)
             [ "19"
             , "diciannove"
             ]
  , examples (NumeralValue 20)
             [ "20"
             , "venti"
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
             , "100 000"
             ]
  , examples (NumeralValue 3000000)
             [ "3M"
             , "3000K"
             , "3000000"
             , "3.000.000"
             , "3 000 000"
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
             , "meno 1.200.000"
             , "negativo 1200000"
             , "-1,2M"
             , "-1200K"
             , "-,0012G"
             ]
  , examples (NumeralValue 6.7)
             [ "6,7"
             ]
  , examples (NumeralValue 6700.54)
             [ "6.700,54"
             , "6 700,54"
             ]
  ]
