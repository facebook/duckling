-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Distance.BG.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Resolve
import Duckling.Distance.Types
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale BG Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Kilometre 3)
             [ "3 километра"
             , "3 км"
             , "3км"
             , "3.0 км"
             ]
  , examples (simple Mile 8)
             [ "8 мили"
             , "осем мили"
             ]
  , examples (simple Metre 1)
             [ "1 м",
               "1 метър",
               "един метър"
             ]
  , examples (simple Centimetre 2)
             [ "2см"
             , "2 сантиметра"
             ]
  , examples (simple Millimetre 4)
             [ "4мм"
             , "4 милиметра"
             ]
  , examples (simple Inch 5)
             [ "5 инча"
             , "5''"
             , "пет инча"
             , "5\""
             ]
  , examples (simple Foot 35)
             [ "35 фута"
             , "35'"
             , "тридесет и пет фута"
             ]
  , examples (simple Yard 47)
             [ "47 ярда"
             , "четиридесет и седем ярда"
             ]
  ]
