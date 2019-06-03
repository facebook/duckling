-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Distance.RO.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Distance.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale RO Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Kilometre 3)
             [ "3 kilometri"
             , "3 km"
             , "3km"
             , "3,0 km"
             ]
  , examples (simple Mile 8)
             [ "8 mile"
             ]
  , examples (simple Metre 9)
             [ "9m"
             , "9 m"
             ]
  , examples (simple Centimetre 2)
             [ "2cm"
             , "2 centimetri"
             ]
  , examples (simple Foot 10)
             [ "zece picioare"
             ]
  , examples (simple Foot 20)
             [ "20 de picioare"
             ]
  ]
