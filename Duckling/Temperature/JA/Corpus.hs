-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Temperature.JA.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Locale
import Duckling.Resolve
import Duckling.Temperature.Types
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale JA Nothing}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (TemperatureValue Celsius 37)
             [ "37°C"
             , "摂氏37°"
             , "摂氏37度"
             ]
  , examples (TemperatureValue Fahrenheit 70)
             [ "70°F"
             , "華氏70°"
             , "華氏70度"
             ]
  , examples (TemperatureValue Degree 45)
             [ "45°"
             , "45度"
             ]
  ]
