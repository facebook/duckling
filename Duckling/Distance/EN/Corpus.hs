-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Distance.EN.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Distance.Types
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (DistanceValue Kilometre 3)
             [ "3 kilometers"
             , "3 km"
             , "3km"
             , "3k"
             , "3.0 km"
             ]
  , examples (DistanceValue Mile 8)
             [ "8 miles"
             , "eight mile"
             , "8 mi"
             ]
  , examples (DistanceValue M 9)
             [ "9m"
             ]
  , examples (DistanceValue Centimetre 2)
             [ "2cm"
             , "2 centimeters"
             ]
  , examples (DistanceValue Inch 5)
             [ "5 in"
             , "5''"
             , "five inches"
             , "5\""
             ]
  ]
