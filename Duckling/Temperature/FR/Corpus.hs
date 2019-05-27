-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Temperature.FR.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Locale
import Duckling.Resolve
import Duckling.Temperature.Types
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale FR Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Celsius 37)
             [ "37°C"
             , "37 ° celsius"
             , "37 degres Celsius"
             , "37 degré C"
             , "trente sept celsius"
             , "37 degré C"
             , "trente sept celsius"
             ]
  , examples (simple Fahrenheit 70)
             [ "70°F"
             , "70 ° Fahrenheit"
             , "70 degrès F"
             , "soixante-dix Fahrenheit"
             ]
  , examples (simple Degree 45)
             [ "45°"
             , "45 degrés"
             , "45 deg."
             ]
  , examples (simple Degree (-10))
             [ "-10°"
             , "- 10 degres"
             , "10 degres en dessous de zero"
             , "10 en dessous de zero"
             ]
  ]
