-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.DA.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale DA Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumeralValue 0)
             [ "0"
             , "nul"
             ]
  , examples (NumeralValue 1)
             [ "1"
             , "én"
             , "en"
             , "ét"
             , "et"
             ]
  , examples (NumeralValue 2)
             [ "2"
             , "to"
             , "et par"
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
             ]
  , examples (NumeralValue 18)
             [ "18"
             , "Atten"
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
             [ "5 tusind"
             , "fem tusinde"
             , "fem Tusind"
             ]
  ]
