-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Distance.ES.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Distance.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale ES Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Kilometre 3)
             [ "3 kilómetros"
             , "3 kilometros"
             , "3 km"
             , "3km"
             , "3k"
             ]
  , examples (simple Kilometre 3.0)
             [ "3,0 km"
             ]
  , examples (simple Mile 8)
             [ "8 miles"
             ]
  , examples (simple Metre 9)
             [ "9m"
             , "9 metros"
             ]
  , examples (simple Centimetre 2)
             [ "2cm"
             , "2 centímetros"
             ]
  ]
