-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Temperature.RO.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Resolve
import Duckling.Temperature.Types
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale RO Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Celsius 37)
             [ "37°C"
             , "37 ° celsius"
             , "37 grade Celsius"
             , "treizeci si sapte celsius"
             , "37 grade Celsius"
             , "37 de grade de Celsius"
             , "treizeci si sapte celsius"
             , "treizeci si sapte de celsius"
             ]
  , examples (simple Fahrenheit 70)
             [ "70°F"
             , "70 ° Fahrenheit"
             , "70 grade F"
             , "saptezeci Fahrenheit"
             ]
  , examples (simple Degree 45)
             [ "45°"
             , "45 grade"
             , "45 de grade"
             ]
  ]
