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
  [ examples (NumeralValue 0)
             [ "0"
             , "null"
             , "keiner"
             , "keinen"
             ]
  , examples (NumeralValue 1)
             [ "1"
             , "eins"
             ]
  , examples (NumeralValue 3)
             [ "3"
             , "Drei"
             ]
  , examples (NumeralValue 8)
             [ "acht"
             ]
  , examples (NumeralValue 18)
             [ "achtzehn"
             ]
  , examples (NumeralValue 30)
             [ "30"
             , "dreissig"
             ]
  , examples (NumeralValue 33)
             [ "33"
             , "drei Und dreissig"
             , "dreiunddreissig"
             , "0033"
             ]
  , examples (NumeralValue 14)
             [ "14"
             , "vierzehn"
             ]
  , examples (NumeralValue 15)
             [ "15"
             , "fünfzehn"
             ]
  , examples (NumeralValue 16)
             [ "16"
             , "sechzehn"
             ]
  , examples (NumeralValue 17)
             [ "17"
             , "Siebzehn"
             ]
  , examples (NumeralValue 18)
             [ "18"
             , "achtzehn"
             ]
  , examples (NumeralValue 200)
             [ "200"
             , "zwei hundert"
             ]
  , examples (NumeralValue 102)
             [ "102"
             , "Hundert zwei"
             ]
  , examples (NumeralValue 1.1)
             [ "1,1"
             , "1 komma 1"
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
             [ "5 tausend"
             , "fünf tausend"
             ]
  , examples (NumeralValue 200000)
             [ "zwei hundert tausend"
             ]
  , examples (NumeralValue 721012)
             [ "sieben hundert einundzwanzig tausend zwölf"
             ]
  , examples (NumeralValue 31256721)
             [ "ein und dreissig millionen zwei hundert sechs und fünfzig tausend sieben hundert ein und zwanzig"
             ]
  , examples (NumeralValue 1416.15)
             [ "1416,15"
             ]
  , examples (NumeralValue 1416.15)
             [ "1.416,15"
             ]
  , examples (NumeralValue 1000000.0)
             [ "1.000.000,00"
             ]
  ]
