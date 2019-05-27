-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Temperature.HI.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Resolve
import Duckling.Temperature.Types
import Duckling.Testing.Types

context :: Context
context = testContext {locale = makeLocale HI Nothing}

corpus :: Corpus
corpus = (context, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Celsius 37)
             [ "37 डिग्री सेल्सीयस"
             , "37 ° सेल्सीयस"
             ]
  , examples (simple Fahrenheit 71)
             [ "71 डिग्री फारेनहाइट"
             , "71 ° फारेनहाइट"
             ]
  , examples (simple Degree 45)
             [ "45 डिग्री"
             , "45 °"
             ]
  ]
