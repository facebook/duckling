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
  [ examples (simple 0)
             [ "0"
             , "nulla"
             , "zéró"
             ]
  , examples (simple 1)
             [ "1"
             , "egy"
             ]
  , examples (simple 2)
             [ "kettő"
             ]
  , examples (simple 3)
             [ "három"
             ]
  , examples (simple 4)
             [ "négy"
             ]
  , examples (simple 5)
             [ "öt"
             ]
  , examples (simple 6)
             [ "hat"
             ]
  , examples (simple 7)
             [ "hét"
             ]
  , examples (simple 8)
             [ "nyolc"
             ]
  , examples (simple 9)
             [ "kilenc"
             ]
  , examples (simple 11)
             [ "tizenegy"
             ]
  , examples (simple 15)
             [ "tizenöt"
             ]
  , examples (simple 17)
             [ "tizenhét"
             ]
  , examples (simple 20)
             [ "20"
             , "húsz"
             ]
  , examples (simple 22)
             [ "huszonkettő"
             ]
  , examples (simple 24)
             [ "24"
             , "huszonnégy"
             ]
  , examples (simple 26)
             [ "huszonhat"
             ]
  , examples (simple 28)
             [ "huszonnyolc"
             ]
  , examples (simple 10)
             [ "tíz"
             ]
  , examples (simple 20)
             [ "húsz"
             ]
  , examples (simple 50)
             [ "ötven"
             ]
  , examples (simple 34)
             [ "harmincnégy"
             ]
  ]
