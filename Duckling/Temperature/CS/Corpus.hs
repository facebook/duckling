-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Temperature.CS.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Resolve
import Duckling.Temperature.Types
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale CS Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Degree 45)
             [ "45°"
             , "45 stupňů"
             , "45 stupnu"
             ]
  , examples (simple Degree 1)
             [ "1°"
             , "1 stupeň"
             , "1 stupen"
             ]
  , examples (simple Degree 2)
             [ "2°"
             , "2 stupně"
             , "2 stupne"
             ]
  , examples (simple Celsius 9)
             [ "9°C"
             , "9 stupňů celsia"
             , "9 stupnu celsia"
             ]
  , examples (simple Degree (-3))
             [ "3 stupně pod nulou"
             , "3 stupne pod nulou"
             ]
  ]
