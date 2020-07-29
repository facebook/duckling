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
  [ examples (simple 0)
             [ "0"
             , "zero"
             , "zéro"
             ]
  , examples (simple 1)
             [ "1"
             , "un"
             , "une"
             ]
  , examples (simple 11)
             [ "onze"
             ]
  , examples (simple 17)
             [ "dix sept"
             , "dix-sept"
             ]
  , examples (simple 21)
             [ "vingt et un"
             , "vingt-et-un"
             ]
  , examples (simple 23)
             [ "vingt trois"
             , "vingt-trois"
             ]
  , examples (simple 70)
             [ "soixante dix"
             ]
  , examples (simple 71)
             [ "soixante onze"
             ]
  , examples (simple 78)
             [ "soixante dix huit"
             ]
  , examples (simple 73)
             [ "soixante treize"
             ]
  , examples (simple 80)
             [ "quatre vingt"
             ]
  , examples (simple 81)
             [ "quatre vingt un"
             ]
  , examples (simple 82)
             [ "quatre vingt deux"
             ]
  , examples (simple 90)
             [ "quatre vingt dix"
             ]
  , examples (simple 91)
             [ "quatre vingt onze"
             ]
  , examples (simple 92)
             [ "quatre vingt douze"
             ]
  , examples (simple 99)
             [ "quatre vingt dix neuf"
             ]
  , examples (simple 33)
             [ "33"
             , "trente trois"
             , "trente-trois"
             , "trente 3"
             ]
  , examples (simple 118)
             [ "cent dix-huit"
             ]
  , examples (simple 4020)
             [ "quatre mille vingt"
             ]
  , examples (simple 100000)
             [ "100.000"
             , "100000"
             , "100K"
             , "100k"
             , "cent mille"
             , "100 000"
             ]
  , examples (simple 3000000)
             [ "3M"
             , "3000K"
             , "3000000"
             , "3.000.000"
             , "trois millions"
             ]
  , examples (simple 1200000)
             [ "1.200.000"
             , "1200000"
             , "1,2M"
             , "1200K"
             , ",0012G"
             , "un million deux cent mille"
             ]
  , examples (simple (-1200000))
             [ "- 1.200.000"
             , "-1200000"
             , "moins 1200000"
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
