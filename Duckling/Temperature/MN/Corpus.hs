-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Temperature.MN.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Locale
import Duckling.Resolve
import Duckling.Temperature.Types
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale MN Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Celsius 37)
             [ "37°C"
             , "37 ° цельс"
             , "37 цельсийн хэм"
             , "гучин долоон хэм"
             , "цельсийн гучин долоо"
             ]
  , examples (simple Fahrenheit 70)
             [ "70°F"
             , "70 ° фарангейт"
             , "70 хэм F"
             , "далан фарангейт"
             ]
  , examples (simple Degree 45)
             [ "45°"
             , "45 хэм"
             , "45 град."
             ]
  , examples (simple Degree (-2))
             [ "-2°"
             , "- 2 хэм"
             , "хасах 2 хэм"
             ]
  , examples (between Degree (30, 40))
             [ "30 хэмээс 40 хэмийн хооронд"
             ]
  , examples (between Celsius (30, 40))
             [ "30 цельсээс 40 цельсийн хооронд"
             , "цельсийн 30 хэмээс 40 хэмийн хооронд"
             , "цельсийн 30-40 хэм"
             ]
    , examples (above Degree 40)
             [ "40 хэмээс их"
             , "дор хаяж 40 хэм"
             , "40 хэмээс дээгүүр"
             ]
    , examples (under Degree 40)
             [ "40 хэмээс доогуур"
             , "40 хэмээс бага"
             ]
  ]
