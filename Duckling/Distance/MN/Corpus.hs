-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Distance.MN.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Resolve
import Duckling.Distance.Types
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale MN Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Kilometre 3)
             [ "3 километр"
             , "3 км"
             , "3км"
             ]
  , examples (simple Mile 8)
             [ "8 миль"
             ]
  , examples (simple Metre 1)
             [ "1 м"
             , "1 метр"
             ]
  , examples (simple Centimetre 2)
             [ "2см"
             ]
  , examples (simple Millimetre 4)
             [ "4мм"
             , "4 миллиметр"
             ]
  , examples (simple Inch 5)
             [ "5 инч"
             , "5\""
             ]
  , examples (simple Foot 35)
             [ "35 фут"
             , "35'"
             ]
  , examples (simple Yard 47)
             [ "47 яард"
             ]
  ]
