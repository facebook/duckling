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
  [ examples (simple 0)
             [ "शून्य"
             , "०"
             ]
  , examples (simple 1)
             [ "एक"
             ]
  , examples (simple 2)
             [ "दो"
             ]
  , examples (simple 3)
             [ "तीन"
             ]
  , examples (simple 4)
             [ "चार"
             ]
  , examples (simple 5)
             [ "पाँच"
             ]
  , examples (simple 6)
             [ "छह"
             ]
  , examples (simple 7)
             [ "सात"
             ]
  , examples (simple 8)
             [ "आठ"
             ]
  , examples (simple 9)
             [ "नौ"
             ]
  , examples (simple 10)
             [ "दस"
             ]
  , examples (simple 15)
             [ "पन्द्रह"
             ]
  , examples (simple 17)
             [ "सत्रह"
             ]
  , examples (simple 20)
             [ "बीस"
             ]
  , examples (simple 22)
             [ "बाईस"
             ]
  , examples (simple 24)
             [ "चौबीस"
             ]
  , examples (simple 26)
             [ "छब्बीस"
             ]
  , examples (simple 28)
             [ "अट्ठाईस"
             ]
  , examples (simple 50)
             [ "५०"
             , "पचास"
             ]
  ]
