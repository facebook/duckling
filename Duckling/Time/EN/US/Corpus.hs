-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.

{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.EN.US.Corpus
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
  , examples (datetime (2013, 11, 28, 0, 0, 0) Day)
             [ "thanksgiving day"
             , "thanksgiving"
             , "thanksgiving 2013"
             , "this thanksgiving"
             , "next thanksgiving day"
             ]
  , examples (datetime (2014, 11, 27, 0, 0, 0) Day)
             [ "thanksgiving of next year"
             , "thanksgiving 2014"
             ]
  , examples (datetime (2012, 11, 22, 0, 0, 0) Day)
             [ "last thanksgiving"
             , "thanksgiving day 2012"
             ]
  , examples (datetime (2016, 11, 24, 0, 0, 0) Day)
             [ "thanksgiving 2016"
             ]
  , examples (datetime (2017, 11, 23, 0, 0, 0) Day)
             [ "thanksgiving 2017"
             ]
  , examples (datetime (2012, 11, 26, 0, 0, 0) Day)
             [ "last cyber monday"
             , "cyber monday 2012"
             ]
  , examples (datetime (2017, 11, 27, 0, 0, 0) Day)
             [ "cyber monday 2017"
             ]
  , examples (datetime (2013, 5, 27, 0, 0, 0) Day)
             [ "memorial day"
             , "Next Memorial Day"
             , "decoration day"
             ]
  , examples (datetime (2012, 5, 28, 0, 0, 0) Day)
             [ "last memorial day"
             , "memorial day of last year"
             ]
  , examples (datetimeInterval ((2013, 5, 24, 18, 0, 0), (2013, 5, 28, 0, 0, 0)) Hour)
             [ "memorial day week-end"
             ]
  , examples (datetime (2013, 7, 4, 0, 0, 0) Day)
             [ "independence day"
             ]
  , examples (datetime (2013, 11, 11, 0, 0, 0) Day)
             [ "veterans day"
             ]
  , examples (datetime (2013, 5, 1, 0, 0, 0) Day)
             [ "law day"
             , "Lei Day"
             , "loyalty day"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "George Washington Day"
             , "washington's birthday"
             , "presidents' day"
             , "president day 2013"
             , "Daisy Gatson Bates Day"
             ]
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "Lincolns birthday"
             , "Abraham Lincoln's birthday"
             , "Lincoln birthday"
             , "Lincolns' birthday"
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
  , examples (datetime (2019, 9, 8, 0, 0, 0) Day)
             [ "national grandparents day 2019"
             ]
  , examples (datetime (2018, 5, 11, 0, 0, 0) Day)
             [ "Military Spouse day 2018"
             ]
  ]
