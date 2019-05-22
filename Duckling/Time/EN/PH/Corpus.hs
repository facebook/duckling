-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.

{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.EN.PH.Corpus
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
  , examples (datetimeHoliday (2013, 11, 28, 0, 0, 0) Day "Thanksgiving Day")
             [ "thanksgiving day"
             , "thanksgiving"
             , "thanksgiving 2013"
             , "this thanksgiving"
             , "next thanksgiving day"
             ]
  , examples (datetimeHoliday (2014, 11, 27, 0, 0, 0) Day "Thanksgiving Day")
             [ "thanksgiving of next year"
             , "thanksgiving 2014"
             ]
  , examples (datetimeHoliday (2012, 11, 22, 0, 0, 0) Day "Thanksgiving Day")
             [ "last thanksgiving"
             , "thanksgiving day 2012"
             ]
  , examples (datetimeHoliday (2016, 11, 24, 0, 0, 0) Day "Thanksgiving Day")
             [ "thanksgiving 2016"
             ]
  , examples (datetimeHoliday (2017, 11, 23, 0, 0, 0) Day "Thanksgiving Day")
             [ "thanksgiving 2017"
             ]
  , examples (datetimeHoliday (2013, 12, 30, 0, 0, 0) Day "Rizal Day")
             [ "Rizal day"
             ]
  , examples (datetimeHoliday (2013, 6, 12, 0, 0, 0) Day "Independence Day")
             [ "Independence day"
             ]
  , examples (datetimeHoliday (2013, 6, 16, 0, 0, 0) Day "Father's Day")
             [ "Father's Day"
             ]
  , examples (datetimeHoliday (2012, 6, 17, 0, 0, 0) Day "Father's Day")
             [ "last fathers day"
             ]
  , examples (datetimeHoliday (1996, 6, 16, 0, 0, 0) Day "Father's Day")
             [ "fathers day 1996"
             ]
  , examples (datetimeHoliday (2013, 5, 12, 0, 0, 0) Day "Mother's Day")
             [ "Mother's Day"
             , "next mothers day"
             ]
  , examples (datetimeHoliday (2012, 5, 13, 0, 0, 0) Day "Mother's Day")
             [ "last mothers day"
             ]
  , examples (datetimeHoliday (2014, 5, 11, 0, 0, 0) Day "Mother's Day")
             [ "mothers day 2014"
             ]
  , examples (datetimeHoliday (2013, 5, 1, 0, 0, 0) Day "Labour Day")
             [ "labour day"
             ]
  , examples (datetimeHoliday (2012, 5, 1, 0, 0, 0) Day "Labour Day")
             [ "labour day of last year"
             , "Labour Day 2012"
             ]
  , examples (datetimeHoliday (2013, 8, 26, 0, 0, 0) Day "National Heroes' Day")
             [ "national heroes' day"
             ]
  ]
