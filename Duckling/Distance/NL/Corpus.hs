-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Distance.NL.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Distance.Types
import Duckling.Lang
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {lang = NL}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (DistanceValue Kilometre 3)
             [ "3 kilometer"
             , "3 km"
             , "3km"
             , "3k"
             ]
  , examples (DistanceValue Kilometre 3.0)
             [ "3,0 km"
             , "3,0km"
             ]
  , examples (DistanceValue Mile 8)
             [ "8 mijl"
             ]
  , examples (DistanceValue Metre 9)
             [ "9m"
             , "9 m"
             , "9 meter"
             ]
  , examples (DistanceValue Centimetre 2)
             [ "2cm"
             , "2 cm"
             , "2 centimeter"
             ]
  ]
