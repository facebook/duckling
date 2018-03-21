-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Distance.SV.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Distance.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale SV Nothing}, allExamples)

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
             ]
  , examples (simple Mil 8)
             [ "8 mil"
             ]
  , examples (simple Metre 9)
             [ "9 meter"
             , "9m"
             ]
  , examples (simple Centimetre 2)
             [ "2cm"
             , "2 centimeter"
             ]
  ]
