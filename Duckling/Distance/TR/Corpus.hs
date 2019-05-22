-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Distance.TR.Corpus
  ( corpus ) where

import Data.String
import Prelude

import Duckling.Distance.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale TR Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Kilometre 3)
             [ "3 Kilometre"
             , "3 kilometre"
             , "3 km"
             , "3km"
             ]
  , examples (simple Kilometre 3.0)
             [ "3,0 km"
             ]
  , examples (simple Mile 8)
             [ "8 Mil"
             , "8 mil"
             ]
  , examples (simple Metre 9)
             [ "9 Metre"
             , "9 metre"
             , "9 m"
             , "9m"
             ]
  , examples (simple Centimetre 2)
             [ "2 Santimetre"
             , "2 santim"
             , "2 cm"
             , "2cm"
             ]
  ]
