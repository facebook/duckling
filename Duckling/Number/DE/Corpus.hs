-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Number.DE.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Lang
import Duckling.Number.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {lang = DE}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumberValue 0)
             [ "0"
             , "null"
             ]
  , examples (NumberValue 1)
             [ "1"
             , "eins"
             ]
  , examples (NumberValue 3)
             [ "3"
             , "Drei"
             ]
  , examples (NumberValue 30)
             [ "30"
             , "dreissig"
             ]
  , examples (NumberValue 33)
             [ "33"
             , "drei Und dreissig"
             , "dreiunddreissig"
             , "0033"
             ]
  , examples (NumberValue 14)
             [ "14"
             , "vierzehn"
             ]
  , examples (NumberValue 16)
             [ "16"
             , "sechzehn"
             ]
  , examples (NumberValue 17)
             [ "17"
             , "Siebzehn"
             ]
  , examples (NumberValue 18)
             [ "18"
             , "achtzehn"
             ]
  , examples (NumberValue 200)
             [ "200"
             , "zwei hundert"
             ]
  , examples (NumberValue 102)
             [ "102"
             , "Hundert zwei"
             ]
  , examples (NumberValue 1.1)
             [ "1,1"
             , "1 komma 1"
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
             , "minus 1.200.000"
             , "negativ 1200000"
             , "-1,2M"
             , "-1200K"
             , "-,0012G"
             ]
  , examples (NumberValue 5000)
             [ "5 tausend"
             , "fünf tausend"
             ]
  , examples (NumberValue 200000)
             [ "zwei hundert tausend"
             ]
  , examples (NumberValue 721012)
             [ "sieben hundert einundzwanzig tausend zwölf"
             ]
  , examples (NumberValue 31256721)
             [ "ein und dreissig millionen zwei hundert sechs und fünfzig tausend sieben hundert ein und zwanzig"
             ]
  , examples (NumberValue 1416.15)
             [ "1416,15"
             ]
  , examples (NumberValue 1416.15)
             [ "1.416,15"
             ]
  , examples (NumberValue 1000000.0)
             [ "1.000.000,00"
             ]
  ]
