-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.DE.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale DE Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple 0)
             [ "0"
             , "null"
             , "keiner"
             , "keinen"
             ]
  , examples (simple 1)
             [ "1"
             , "eins"
             ]
  , examples (simple 3)
             [ "3"
             , "Drei"
             ]
  , examples (simple 8)
             [ "acht"
             ]
  , examples (simple 18)
             [ "achtzehn"
             ]
  , examples (simple 30)
             [ "30"
             , "dreissig"
             ]
  , examples (simple 33)
             [ "33"
             , "drei Und dreissig"
             , "dreiunddreissig"
             , "0033"
             ]
  , examples (simple 14)
             [ "14"
             , "vierzehn"
             ]
  , examples (simple 15)
             [ "15"
             , "fünfzehn"
             ]
  , examples (simple 16)
             [ "16"
             , "sechzehn"
             ]
  , examples (simple 17)
             [ "17"
             , "Siebzehn"
             ]
  , examples (simple 18)
             [ "18"
             , "achtzehn"
             ]
  , examples (simple 200)
             [ "200"
             , "zwei hundert"
             ]
  , examples (simple 102)
             [ "102"
             , "Hundert zwei"
             ]
  , examples (simple 1.1)
             [ "1,1"
             , "1 komma 1"
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
             ]
  , examples (simple 3000000)
             [ "3M"
             , "3000K"
             , "3000000"
             , "3.000.000"
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
             , "minus 1.200.000"
             , "negativ 1200000"
             , "-1,2M"
             , "-1200K"
             , "-,0012G"
             ]
  , examples (simple 5000)
             [ "5 tausend"
             , "fünf tausend"
             ]
  , examples (simple 200000)
             [ "zwei hundert tausend"
             ]
  , examples (simple 721012)
             [ "sieben hundert einundzwanzig tausend zwölf"
             ]
  , examples (simple 31256721)
             [ "ein und dreissig millionen zwei hundert sechs und fünfzig tausend sieben hundert ein und zwanzig"
             ]
  , examples (simple 1416.15)
             [ "1416,15"
             ]
  , examples (simple 1416.15)
             [ "1.416,15"
             ]
  , examples (simple 1000000.0)
             [ "1.000.000,00"
             ]
  ]
