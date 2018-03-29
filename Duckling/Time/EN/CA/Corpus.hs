-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.

{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.EN.CA.Corpus
  ( allExamples
  ) where

import Data.String
import Prelude

import Duckling.Testing.Types hiding (examples)
import Duckling.Time.Corpus
import Duckling.Time.Types hiding (Month)
import Duckling.TimeGrain.Types hiding (add)

allExamples :: [Example]
allExamples = concat
  [ examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "2/15"
             , "on 2/15"
             , "2 / 15"
             , "2-15"
             , "2 - 15"
             ]
  , examples (datetime (1974, 10, 31, 0, 0, 0) Day)
             [ "10/31/1974"
             , "10/31/74"
             , "10-31-74"
             , "10.31.1974"
             , "10 31 1974"
             ]
  , examples (datetime (2013, 4, 25, 16, 0, 0) Minute)
             [ "4/25 at 4:00pm"
             ]
  , examples (datetime (2013, 10, 14, 0, 0, 0) Day)
             [ "thanksgiving day"
             , "thanksgiving"
             , "thanksgiving 2013"
             , "this thanksgiving"
             , "next thanksgiving day"
             ]
  , examples (datetime (2014, 10, 13, 0, 0, 0) Day)
             [ "thanksgiving of next year"
             , "thanksgiving 2014"
             ]
  , examples (datetime (2012, 10, 8, 0, 0, 0) Day)
             [ "last thanksgiving"
             , "thanksgiving day 2012"
             ]
  , examples (datetime (2016, 10, 10, 0, 0, 0) Day)
             [ "thanksgiving 2016"
             ]
  , examples (datetime (2017, 10, 9, 0, 0, 0) Day)
             [ "thanksgiving 2017"
             ]
  , examples (datetime (2013, 7, 1, 0, 0, 0) Day)
             [ "canada day"
             , "memorial day"
             , "dominion day"
             , "Next Memorial Day"
             ]
  , examples (datetime (2013, 4, 9, 0, 0, 0) Day)
             [ "vimy ridge day"
             ]
  , examples (datetime (2013, 7, 12, 0, 0, 0) Day)
             [ "the Glorious Twelfth"
             , "Orangemen's Day"
             , "the twelfth"
             ]
  , examples (datetime (2013, 6, 16, 0, 0, 0) Day)
             [ "Father's Day"
             ]
  , examples (datetime (2012, 6, 17, 0, 0, 0) Day)
             [ "last fathers day"
             ]
  , examples (datetime (1996, 6, 16, 0, 0, 0) Day)
             [ "fathers day 1996"
             ]
  , examples (datetime (2018, 5, 21, 0, 0, 0) Day)
             [ "Victoria day 2018"
             , "Sovereign's birthday 2018"
             ]
  , examples (datetime (2013, 8, 5, 0, 0, 0) Day)
             [ "Civic Holiday"
             , "British Columbia Day"
             , "Natal Day"
             , "New Brunswick Day"
             , "Saskatchewan Day"
             , "Terry Fox Day"
             ]
  , examples (datetime (2018, 2, 19, 0, 0, 0) Day)
             [ "family day 2018"
             , "islander day 2018"
             , "louis riel day 2018"
             , "nova scotia heritage day 2018"
             ]
  ]
