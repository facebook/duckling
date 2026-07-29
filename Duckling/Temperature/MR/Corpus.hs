-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Temperature.MR.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Resolve
import Duckling.Temperature.Types
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale MR Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Celsius 37)
             [ "37 डिग्री सेल्यियस"
             , "37 सेल्यियस"
             ]
  , examples (simple Fahrenheit 71)
             [ "71 डिग्री फॅरेनहाइट"
             ]
  , examples (simple Degree 45)
             [ "45 डिग्री"
             ]
  ]
