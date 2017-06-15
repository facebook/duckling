-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Distance.TR.Corpus
  ( corpus ) where

import Data.String
import Prelude

import Duckling.Distance.Types
import Duckling.Lang
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {lang = TR}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (DistanceValue Kilometre 3)
             [ "3 Kilometre"
             , "3 kilometre"
             , "3 km"
             , "3km"
             ]
  , examples (DistanceValue Kilometre 3.0)
             [ "3,0 km"
             ]
  , examples (DistanceValue Mile 8)
             [ "8 Mil"
             , "8 mil"
             ]
  , examples (DistanceValue Metre 9)
             [ "9 Metre"
             , "9 metre"
             , "9 m"
             , "9m"
             ]
  , examples (DistanceValue Centimetre 2)
             [ "2 Santimetre"
             , "2 santim"
             , "2 cm"
             , "2cm"
             ]
  ]
