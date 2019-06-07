-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.NE.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale NE Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumeralValue 0)
             [ "शुन्य"
             , "सुन्ना"
             ]
  , examples (NumeralValue 1)
             [ "एक"
             ]
  , examples (NumeralValue 2)
             [ "दुई"
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
             [ "छ"
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
             [ "दश"
             ]
  , examples (NumeralValue 11)
             [ "एघार"
             ]
  , examples (NumeralValue 12)
             [ "बाह्र"
             ]
  , examples (NumeralValue 20)
             [ "बिस"
             ]
  , examples (NumeralValue 21)
             [ "एक्काइस"
             ]
  , examples (NumeralValue 22)
             [ "बाइस"
             ]
  , examples (NumeralValue 26)
             [ "छब्बिस"
             ]
  , examples (NumeralValue 30)
             [ "तिस"
             ]
  , examples (NumeralValue 50)
             [ "पचास"
             ]
  ]
