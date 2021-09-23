-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Temperature.CA.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Locale
import Duckling.Resolve
import Duckling.Temperature.Types
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale CA Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Celsius 37)
             [ "37°C"
             , "37 ° celsius"
             , "37 graus Celsius"
             , "37 graus C"
             , "37 centígrads"
             , "37 graus centígrads"
             , "37 Celsius"
             ]
  , examples (simple Fahrenheit 70)
             [ "70°F"
             , "70 ° Fahrenheit"
             , "70 graus F"
             , "setanta Fahrenheit"
             ]
  , examples (simple Degree 45)
             [ "45°"
             , "45 graus"
             ]
  , examples (simple Degree 37)
             [ "trenta-set graus"
             , "37 grads"
             ]
  , examples (simple Degree (-10))
             [ "-10°"
             , "- deu graus"
             , "10 sota zero"
             ]
  ]
