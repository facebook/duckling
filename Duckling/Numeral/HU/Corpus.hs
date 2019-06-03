-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.HU.Corpus
  ( corpus ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale HU Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumeralValue 0)
             [ "0"
             , "nulla"
             , "zéró"
             ]
  , examples (NumeralValue 1)
             [ "1"
             , "egy"
             ]
  , examples (NumeralValue 2)
             [ "kettő"
             ]
  , examples (NumeralValue 3)
             [ "három"
             ]
  , examples (NumeralValue 4)
             [ "négy"
             ]
  , examples (NumeralValue 5)
             [ "öt"
             ]
  , examples (NumeralValue 6)
             [ "hat"
             ]
  , examples (NumeralValue 7)
             [ "hét"
             ]
  , examples (NumeralValue 8)
             [ "nyolc"
             ]
  , examples (NumeralValue 9)
             [ "kilenc"
             ]
  , examples (NumeralValue 11)
             [ "tizenegy"
             ]
  , examples (NumeralValue 15)
             [ "tizenöt"
             ]
  , examples (NumeralValue 17)
             [ "tizenhét"
             ]
  , examples (NumeralValue 20)
             [ "20"
             , "húsz"
             ]
  , examples (NumeralValue 22)
             [ "huszonkettő"
             ]
  , examples (NumeralValue 24)
             [ "24"
             , "huszonnégy"
             ]
  , examples (NumeralValue 26)
             [ "huszonhat"
             ]
  , examples (NumeralValue 28)
             [ "huszonnyolc"
             ]
  , examples (NumeralValue 10)
             [ "tíz"
             ]
  , examples (NumeralValue 20)
             [ "húsz"
             ]
  , examples (NumeralValue 50)
             [ "ötven"
             ]
  , examples (NumeralValue 34)
             [ "harmincnégy"
             ]
  ]
