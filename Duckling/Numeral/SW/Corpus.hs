-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.SW.Corpus
  ( corpus ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale SW Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple 0)
             [ "0"
             , "sufuri"
             , "zero"
             ]
  , examples (simple 1)
             [ "moja"
             ]
  , examples (simple 2)
             [ "mbili"
             ]
  , examples (simple 3)
             [ "tatu"
             ]
  , examples (simple 4)
             [ "nne"
             ]
  , examples (simple 5)
             [ "tano"
             ]
  , examples (simple 6)
             [ "sita"
             ]
  , examples (simple 7)
             [ "saba"
             ]
  , examples (simple 8)
             [ "nane"
             ]
  , examples (simple 9)
             [ "tisa"
             ]
  , examples (simple 10)
             [ "kumi"
             ]
  , examples (simple 20)
             [ "ishirini"
             ]
  , examples (simple 30)
             [ "thelathini"
             ]
  , examples (simple 40)
             [ "arubaini"
             , "arobaini"
             ]
  , examples (simple 50)
             [ "hamsini"
             ]
  , examples (simple 60)
             [ "sitini"
             ]
  , examples (simple 70)
             [ "sabini"
             ]
  , examples (simple 80)
             [ "themanini"
             ]
  , examples (simple 90)
             [ "tisini"
             ]
  ]
