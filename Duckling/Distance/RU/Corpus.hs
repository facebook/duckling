-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Distance.RU.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Resolve
import Duckling.Distance.Types
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale RU Nothing}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (DistanceValue Kilometre 3)
             [ "3 километра"
             , "3 км"
             , "3км"
             , "3.0 км"
             ]
  , examples (DistanceValue Mile 8)
             [ "8 миль"
             , "восемь миль"
             ]
  , examples (DistanceValue Metre 1)
             [ "1 м",
               "1 метр",
               "один метр"
             ]
  , examples (DistanceValue Centimetre 2)
             [ "2см"
             , "2 сантиметра"
             ]
  , examples (DistanceValue Millimetre 4)
             [ "4мм"
             , "4 миллиметра"
             ]
  , examples (DistanceValue Inch 5)
             [ "5 дюймов"
             , "5''"
             , "пять дюймов"
             , "5\""
             ]
  , examples (DistanceValue Foot 35)
             [ "35 футов"
             , "35'"
             , "тридцать пять футов"
             ]
  , examples (DistanceValue Yard 47)
             [ "47 ярдов"
             , "сорок семь ярдов"
             ]
  ]
