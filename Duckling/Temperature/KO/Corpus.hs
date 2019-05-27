-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Temperature.KO.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Locale
import Duckling.Resolve
import Duckling.Temperature.Types
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale KO Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Celsius 37)
             [ "37°C"
             , "섭씨37°"
             , "섭씨37도"
             ]
  , examples (simple Fahrenheit 70)
             [ "70°F"
             , "화씨70°"
             , "화씨70도"
             ]
  , examples (simple Degree 45)
             [ "45°"
             , "45도"
             ]
  ]
