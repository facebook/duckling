-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Temperature.GA.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Locale
import Duckling.Resolve
import Duckling.Temperature.Types
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale GA Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (TemperatureValue Celsius 37)
             [ "37°C"
             , "37 ° celsius"
             , "37 céimeanna Celsius"
             , "37 céimeanna C"
             , "37 céimeanna ceinteagrád"
             ]
  , examples (TemperatureValue Fahrenheit 70)
             [ "70°F"
             , "70 ° Fahrenheit"
             , "70 céimeanna F"
             ]
  , examples (TemperatureValue Degree 45)
             [ "45°"
             , "45 céimeanna"
             ]
  , examples (TemperatureValue Degree (-10))
             [ "-10°"
             , "- 10 céimeanna"
             , "10 céimeanna faoi bhun náid"
             , "10 faoi bhun náid"
             ]
  ]
