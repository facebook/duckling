-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.HI.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale HI Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumeralValue 0)
             [ "शून्य"
             , "०"
             ]
  , examples (NumeralValue 1)
             [ "एक"
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
  , examples (NumeralValue 15)
             [ "पन्द्रह"
             ]
  , examples (NumeralValue 17)
             [ "सत्रह"
             ]
  , examples (NumeralValue 20)
             [ "बीस"
             ]
  , examples (NumeralValue 22)
             [ "बाईस"
             ]
  , examples (NumeralValue 24)
             [ "चौबीस"
             ]
  , examples (NumeralValue 26)
             [ "छब्बीस"
             ]
  , examples (NumeralValue 28)
             [ "अट्ठाईस"
             ]
  , examples (NumeralValue 50)
             [ "५०"
             , "पचास"
             ]
  ]
