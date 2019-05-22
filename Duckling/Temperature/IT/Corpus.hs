-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.

{-# LANGUAGE OverloadedStrings #-}

module Duckling.Temperature.IT.Corpus
  ( corpus ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Resolve
import Duckling.Temperature.Types
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale IT Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Celsius 37)
             [ "37°C"
             , "37 ° celsius"
             , "37 ° centigradi"
             , "37 gradi Celsius"
             , "37 gradi Centigradi"
             , "trentasette celsius"
             , "trentasette gradi centigradi"
             ]
  , examples (simple Celsius 1)
             [ "1 grado centigrado"
             ]
  , examples (simple Fahrenheit 70)
             [ "70°F"
             , "70 ° Fahrenheit"
             , "70 gradi F"
             , "70 gradi Fahreneit"
             , "settanta Fahrenheit"
             ]
  , examples (simple Degree 45)
             [ "45°"
             , "45 gradi"
             ]
  , examples (simple Degree 1)
             [ "1 grado"
             ]
  ]
