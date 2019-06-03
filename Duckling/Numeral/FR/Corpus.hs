-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.FR.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale FR Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumeralValue 0)
             [ "0"
             , "zero"
             , "zéro"
             ]
  , examples (NumeralValue 1)
             [ "1"
             , "un"
             , "une"
             ]
  , examples (NumeralValue 11)
             [ "onze"
             ]
  , examples (NumeralValue 17)
             [ "dix sept"
             , "dix-sept"
             ]
  , examples (NumeralValue 21)
             [ "vingt et un"
             , "vingt-et-un"
             ]
  , examples (NumeralValue 23)
             [ "vingt trois"
             , "vingt-trois"
             ]
  , examples (NumeralValue 70)
             [ "soixante dix"
             ]
  , examples (NumeralValue 71)
             [ "soixante onze"
             ]
  , examples (NumeralValue 78)
             [ "soixante dix huit"
             ]
  , examples (NumeralValue 73)
             [ "soixante treize"
             ]
  , examples (NumeralValue 80)
             [ "quatre vingt"
             ]
  , examples (NumeralValue 81)
             [ "quatre vingt un"
             ]
  , examples (NumeralValue 82)
             [ "quatre vingt deux"
             ]
  , examples (NumeralValue 90)
             [ "quatre vingt dix"
             ]
  , examples (NumeralValue 91)
             [ "quatre vingt onze"
             ]
  , examples (NumeralValue 92)
             [ "quatre vingt douze"
             ]
  , examples (NumeralValue 99)
             [ "quatre vingt dix neuf"
             ]
  , examples (NumeralValue 33)
             [ "33"
             , "trente trois"
             , "trente-trois"
             , "trente 3"
             ]
  , examples (NumeralValue 118)
             [ "cent dix-huit"
             ]
  , examples (NumeralValue 4020)
             [ "quatre mille vingt"
             ]
  , examples (NumeralValue 100000)
             [ "100.000"
             , "100000"
             , "100K"
             , "100k"
             , "cent mille"
             , "100 000"
             ]
  , examples (NumeralValue 3000000)
             [ "3M"
             , "3000K"
             , "3000000"
             , "3.000.000"
             , "trois millions"
             ]
  , examples (NumeralValue 1200000)
             [ "1.200.000"
             , "1200000"
             , "1,2M"
             , "1200K"
             , ",0012G"
             , "un million deux cent mille"
             ]
  , examples (NumeralValue (-1200000))
             [ "- 1.200.000"
             , "-1200000"
             , "moins 1200000"
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
