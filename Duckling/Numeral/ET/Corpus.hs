-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.ET.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale ET Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumeralValue 0)
             [ "0"
             , "null"
             ]
  , examples (NumeralValue 1)
             [ "1"
             , "üks"
             ]
  , examples (NumeralValue 33)
             [ "33"
             , "Kolmkümmend kolm"
             ]
  , examples (NumeralValue 14)
             [ "14"
             , "neliteist"
             ]
  , examples (NumeralValue 16)
             [ "16"
             , "kuusteist"
             ]
  , examples (NumeralValue 17)
             [ "17"
             , "Seitseteist"
             ]
  , examples (NumeralValue 18)
             [ "18"
             , "kaheksateist"
             ]
  , examples (NumeralValue 1.1)
             [ "1.1"
             , "1.10"
             , "01.10"
             ]
  , examples (NumeralValue 0.77)
             [ "0.77"
             , ".77"
             ]
  , examples (NumeralValue 100000)
             [ "100,000"
             , "100000"
             , "100K"
             , "100k"
             , "100 000"
             ]
  , examples (NumeralValue 3000000)
             [ "3M"
             , "3000K"
             , "3000000"
             , "3,000,000"
             , "3 000 000"
             ]
  , examples (NumeralValue 1200000)
             [ "1,200,000"
             , "1200000"
             , "1.2M"
             , "1200K"
             , ".0012G"
             , "1 200 000"
             ]
  , examples (NumeralValue (-1200000))
             [ "- 1,200,000"
             , "-1200000"
             , "miinus 1,200,000"
             , "-1.2M"
             , "-1200K"
             , "-.0012G"
             ]
  , examples (NumeralValue 5000)
             [ "viis tuhat"
             ]
  , examples (NumeralValue 200000)
             [ "kakssada tuhat"
             ]
  , examples (NumeralValue 21011)
             [ "kakskümmend üks Tuhat üksteist"
             ]
  , examples (NumeralValue 721012)
             [ "seitsesada kakskümmend üks tuhat kaksteist"
             ]
  , examples (NumeralValue 31256721)
             [ "kolmkümmend üks miljonit kakssada viiskümmend kuus tuhat seitsesada kakskümmend üks"
             ]
  ]
