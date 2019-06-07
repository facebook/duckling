-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Distance.CS.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Distance.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale CS Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Kilometre 3)
             [ "3 kilometry"
             , "3 km"
             , "3km"
             ]
  , examples (simple Mile 8)
             [ "8 mil"
             , "osm mil"
             ]
  , examples (simple Metre 1)
             [ "1m"
             , "1 metr"
             ]
  , examples (simple Metre 2)
             [ "2m"
             , "2 metry"
             ]
  , examples (simple Metre 9)
             [ "9m"
             , "9 metrů"
             ]
  , examples (simple Centimetre 1)
             [ "1cm"
             , "1 centimetr"
             ]
  , examples (simple Centimetre 2)
             [ "2cm"
             , "2 centimetry"
             ]
  , examples (simple Centimetre 9)
             [ "9cm"
             , "9 centimetrů"
             ]
  ]
