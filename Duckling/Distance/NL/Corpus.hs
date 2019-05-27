-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Distance.NL.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Distance.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale NL Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Kilometre 3)
             [ "3 kilometer"
             , "3 km"
             , "3km"
             , "3k"
             ]
  , examples (simple Kilometre 3.0)
             [ "3,0 km"
             , "3,0km"
             ]
  , examples (simple Mile 8)
             [ "8 mijl"
             ]
  , examples (simple Metre 9)
             [ "9m"
             , "9 m"
             , "9 meter"
             ]
  , examples (simple Centimetre 2)
             [ "2cm"
             , "2 cm"
             , "2 centimeter"
             ]
  ]
