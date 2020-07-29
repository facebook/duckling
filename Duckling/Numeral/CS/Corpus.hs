-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.CS.Corpus
  ( corpus ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale CS Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple 0)
             [ "0"
             , "nula"
             ]
  , examples (simple 1)
             [ "1"
             , "jeden"
             , "jedna"
             , "jedno"
             ]
  , examples (simple 2)
             [ "dva"
             , "dvĕ"
             ]
  , examples (simple 3)
             [ "tři"
             ]
  , examples (simple 4)
             [ "čtyři"
             ]
  ]
