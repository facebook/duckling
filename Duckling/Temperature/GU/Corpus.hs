-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Temperature.GU.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Resolve
import Duckling.Temperature.Types
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale GU Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Celsius 37)
             [ "37 ડિગ્રી સેલ્સિયસ"
             , "37 સેલ્સિયસ"
             ]
  , examples (simple Fahrenheit 71)
             [ "71 ડિગ્રી ફેરેનહીટ"
             ]
  , examples (simple Degree 45)
             [ "45 ડિગ્રી"
             ]
  ]
