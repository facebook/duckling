-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Temperature.EN.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Temperature.Types
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (TemperatureValue Celsius 37)
             [ "37°C"
             , "37 ° celsius"
             , "37 degrees Celsius"
             , "thirty seven celsius"
             , "37 degrees Celsius"
             , "thirty seven celsius"
             ]
  , examples (TemperatureValue Fahrenheit 70)
             [ "70°F"
             , "70 ° Fahrenheit"
             , "70 degrees F"
             , "seventy Fahrenheit"
             ]
  , examples (TemperatureValue Degree 45)
             [ "45°"
             , "45 degrees"
             , "45 deg."
             ]
  , examples (TemperatureValue Degree (-2))
             [ "-2°"
             , "- 2 degrees"
             , "2 degrees below zero"
             , "2 below zero"
             ]
  ]
