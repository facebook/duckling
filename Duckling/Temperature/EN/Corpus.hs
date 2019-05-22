-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


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
  [ examples (simple Celsius 37)
             [ "37°C"
             , "37 ° celsius"
             , "37 degrees Celsius"
             , "thirty seven celsius"
             , "37 degrees Celsius"
             , "thirty seven celsius"
             ]
  , examples (simple Fahrenheit 70)
             [ "70°F"
             , "70 ° Fahrenheit"
             , "70 degrees F"
             , "seventy Fahrenheit"
             ]
  , examples (simple Degree 45)
             [ "45°"
             , "45 degrees"
             , "45 deg."
             ]
  , examples (simple Degree (-2))
             [ "-2°"
             , "- 2 degrees"
             , "2 degrees below zero"
             , "2 below zero"
             ]
  , examples (between Degree (30, 40))
             [ "between 30 and 40 degrees"
             , "from 30 degrees to 40 degrees"
             ]
  , examples (between Celsius (30, 40))
             [ "between 30 and 40 celsius"
             , "from 30 celsius and 40 celsius"
             , "between 30 and 40 degrees celsius"
             , "from 30 degrees celsius to 40 degrees celsius"
             , "30-40 degrees celsius"
             ]
    , examples (above Degree 40)
             [ "over 40 degrees"
             , "at least 40 degrees"
             , "more than 40 degrees"
             ]
    , examples (under Degree 40)
             [ "under 40 degrees"
             , "less than 40 degrees"
             , "lower than 40 degrees"
             ]
  ]
