-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Temperature.AR.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Resolve
import Duckling.Temperature.Types
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale AR Nothing}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (TemperatureValue Celsius 37)
             [ "37° سلزيوس"
             , "37 ° سلزيوس"
             , "37 درجة سلزيوس"
             , "سبع وثلاثون سلزيوس"
             ]
  , examples (TemperatureValue Fahrenheit 70)
             [ "70° فهرنهايت"
             , "70 درجة فهرنهايت"
             , "سبعون فهرنهايت"
             ]
  , examples (TemperatureValue Degree 45)
             [ "45°"
             , "45 درجة"
             , "45 درجه مئوية"
             ]
  , examples (TemperatureValue Degree (-2))
             [ "-2°"
             , "- 2 درجة"
             , "درجتين تحت الصفر"
             , "2 تحت الصفر"
             ]
  ]
