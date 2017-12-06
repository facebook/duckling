-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.

-------------------------WORK IN PROGRESS------------------------


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.HI.Corpus
  ( corpus ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale HI Nothing}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumeralValue 0)
             [ "0"
             , "शून्य"
             ]
  , examples (NumeralValue 1)
             [ "1"
             , "एक"
             ]
  , examples (NumeralValue 2)
             [ "दो"
             ]
  , examples (NumeralValue 3)
             [ "तीन"
             ]
  , examples (NumeralValue 4)
             [ "चार"
             ]
  , examples (NumeralValue 5)
             [ "पाँच"
             ]
  , examples (NumeralValue 6)
             [ "छह"
             ]
  , examples (NumeralValue 7)
             [ "सात"
             ]
  , examples (NumeralValue 8)
             [ "आठ"
             ]
  , examples (NumeralValue 9)
             [ "नौ"
             ]
  , examples (NumeralValue 10)
             [ "दस"
             ]
--  , examples (NumeralValue 15)
--             [ "tizenöt"
--             ]
--  , examples (NumeralValue 17)
--             [ "tizenhét"
--             ]
--  , examples (NumeralValue 20)
--             [ "20"
--             , "húsz"
--             ]
--  , examples (NumeralValue 22)
--             [ "huszonkettő"
--             ]
--  , examples (NumeralValue 24)
--             [ "24"
--             , "huszonnégy"
--             ]
--  , examples (NumeralValue 26)
--             [ "huszonhat"
--             ]
--  , examples (NumeralValue 28)
--             [ "huszonnyolc"
--             ]
--  , examples (NumeralValue 10)
--             [ "tíz"
--             ]
--  , examples (NumeralValue 20)
--             [ "húsz"
--             ]
--  , examples (NumeralValue 50)
--             [ "ötven"
--             ]
--  , examples (NumeralValue 34)
--             [ "harmincnégy"
--             ]
  ]
-----------------------------------------------------------------