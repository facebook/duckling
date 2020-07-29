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
  [ examples (simple 0)
             [ "शुन्य"
             , "सुन्ना"
             ]
  , examples (simple 1)
             [ "एक"
             ]
  , examples (simple 2)
             [ "दुई"
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
             [ "छ"
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
             [ "दश"
             ]
  , examples (simple 11)
             [ "एघार"
             ]
  , examples (simple 12)
             [ "बाह्र"
             ]
  , examples (simple 20)
             [ "बिस"
             ]
  , examples (simple 21)
             [ "एक्काइस"
             ]
  , examples (simple 22)
             [ "बाइस"
             ]
  , examples (simple 26)
             [ "छब्बिस"
             ]
  , examples (simple 30)
             [ "तिस"
             ]
  , examples (simple 50)
             [ "पचास"
             ]
  ]
