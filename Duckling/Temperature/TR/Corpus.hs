-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Temperature.TR.Corpus
  ( corpus ) where

import Prelude
import Data.String
import Duckling.Locale
import Duckling.Resolve
import Duckling.Temperature.Types
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale TR Nothing}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (TemperatureValue Celsius 37)
             [ "37°C"
             , "37 ° santigrat"
             , "37 derece C"
             , "37 C"
             , "37 derece santigrat"
             , "otuz yedi santigrat"
             ]
  , examples (TemperatureValue Fahrenheit 70)
             [ "70°F"
             , "70 ° Fahrenhayt"
             , "70 ° Fahrenayt"
             , "70 derece fahrenhayt"
             , "70 derece F"
             , "70 F"
             , "yetmiş fahrenayt"
             ]
  , examples (TemperatureValue Degree 45)
             [ "45°"
             , "45 derece"
             , "kırk beş derece"
             ]
  , examples (TemperatureValue Degree (-2))
             [ "-2°"
             , "- 2 derece"
             , "sıfırın altında 2°"
             , "sıfırın altında 2 derece"
             , "2° sıfırın altında"
             , "2 derece sıfırın altında"
             ]
  ]
