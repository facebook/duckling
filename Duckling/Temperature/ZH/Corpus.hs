-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Temperature.ZH.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Locale
import Duckling.Resolve
import Duckling.Temperature.Types
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale ZH Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Celsius 37)
             [ "37°C"
             , "摄氏37°"
             , "攝氏37°"
             , "摄氏37度"
             , "攝氏37度"
             , "37摄氏°"
             , "37攝氏°"
             , "37摄氏度"
             , "37攝氏度"
             ]
  , examples (simple Fahrenheit 70)
             [ "70°F"
             , "华氏70°"
             , "華氏70°"
             , "华氏70度"
             , "華氏70度"
             , "70华氏°"
             , "70華氏°"
             , "70华氏度"
             , "70華氏度"
             ]
  , examples (simple Degree 45)
             [ "45°"
             , "45度"
             ]
  ]
