-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.MR.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale MR Nothing}, testOptions, allExamples)

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
             [ "दोन"
             ]
  , examples (NumeralValue 3)
             [ "तीन"
             ]
  , examples (NumeralValue 4)
             [ "चार"
             ]
  , examples (NumeralValue 5)
             [ "पाच"
             ]
  , examples (NumeralValue 1.5)
             [ "एक पॉइंट पाच"
             , "1.5"
             , "१.५"
             ]
  , examples (NumeralValue 6)
             [ "सहा"
             , "६"
             ]
  ]
