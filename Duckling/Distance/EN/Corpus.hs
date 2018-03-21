-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Distance.EN.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Distance.Types
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Kilometre 3)
             [ "3 kilometers"
             , "3 km"
             , "3km"
             , "3k"
             , "3.0 km"
             ]
  , examples (simple Mile 8)
             [ "8 miles"
             , "eight mile"
             , "8 mi"
             ]
  , examples (simple M 9)
             [ "9m"
             ]
  , examples (simple Centimetre 2)
             [ "2cm"
             , "2 centimeters"
             ]
  , examples (simple Inch 5)
             [ "5 in"
             , "5''"
             , "five inches"
             , "5\""
             ]
  , examples (simple Metre 1.87)
             [ "1.87 meters"
             ]
  , examples (between Kilometre (3, 5))
             [ "between 3 and 5 kilometers"
             , "from 3km to 5km"
             , "around 3-5 kilometers"
             , "about 3km-5km"
             , "3-5 kilometers"
             ]
  , examples (under Mile 3.5)
             [ "under 3.5 miles"
             , "less than 3.5mi"
             , "lower than three point five miles"
             ]
  , examples (above Inch 5)
             [ "more than five inches"
             , "at least 5''"
             , "over 5\""
             , "above 5 in"
             ]
  ]
