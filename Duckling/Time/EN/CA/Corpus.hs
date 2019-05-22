-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.

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
  , examples (datetimeHoliday (2013, 10, 14, 0, 0, 0) Day "Thanksgiving Day")
             [ "thanksgiving day"
             , "thanksgiving"
             , "thanksgiving 2013"
             , "this thanksgiving"
             , "next thanksgiving day"
             ]
  , examples (datetimeHoliday (2014, 10, 13, 0, 0, 0) Day "Thanksgiving Day")
             [ "thanksgiving of next year"
             , "thanksgiving 2014"
             ]
  , examples (datetimeHoliday (2012, 10, 8, 0, 0, 0) Day "Thanksgiving Day")
             [ "last thanksgiving"
             , "thanksgiving day 2012"
             ]
  , examples (datetimeHoliday (2016, 10, 10, 0, 0, 0) Day "Thanksgiving Day")
             [ "thanksgiving 2016"
             ]
  , examples (datetimeHoliday (2017, 10, 9, 0, 0, 0) Day "Thanksgiving Day")
             [ "thanksgiving 2017"
             ]
  , examples (datetimeHoliday (2013, 7, 1, 0, 0, 0) Day "Memorial Day")
             [ "canada day"
             , "memorial day"
             , "dominion day"
             , "Next Memorial Day"
             ]
  , examples (datetimeHoliday (2013, 4, 9, 0, 0, 0) Day "Vimy Ridge Day")
             [ "vimy ridge day"
             ]
  , examples (datetimeHoliday (2013, 7, 12, 0, 0, 0) Day "The Twelfth")
             [ "the Glorious Twelfth"
             , "Orangemen's Day"
             , "the twelfth"
             ]
  , examples (datetimeHoliday (2013, 6, 24, 0, 0, 0) Day "Discovery Day")
             [ "discovery day"
             , "next discovery  day"
             ]
  , examples (datetimeHoliday (2018, 6, 25, 0, 0, 0) Day "Discovery Day")
             [ "discovery day 2018"
             ]
  , examples (datetimeHoliday (2020, 6, 22, 0, 0, 0) Day "Discovery Day")
             [ "discovery   day 2020"
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
  , examples (datetimeHoliday (2018, 5, 21, 0, 0, 0) Day "Victoria Day")
             [ "Victoria day 2018"
             , "Sovereign's birthday 2018"
             ]
  , examples (datetimeHoliday (2013, 8, 5, 0, 0, 0) Day "Civic Holiday")
             [ "Civic Holiday"
             , "British Columbia Day"
             , "Natal Day"
             , "New Brunswick Day"
             , "Saskatchewan Day"
             , "Terry Fox Day"
             ]
  , examples (datetimeHoliday (2018, 2, 19, 0, 0, 0) Day "Family Day")
             [ "family day 2018"
             , "islander day 2018"
             , "louis riel day 2018"
             , "nova scotia heritage day 2018"
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
  , examples (datetimeHoliday (2017, 5, 22, 0, 0, 0) Day "National Patriots' Day")
             [ "national patriots' day 2017"
             ]
  , examples (datetimeHoliday (2018, 5, 21, 0, 0, 0) Day "National Patriots' Day")
             [ "national patriots' day 2018"
             ]
  , examples (datetimeHoliday (2019, 5, 20, 0, 0, 0) Day "National Patriots' Day")
             [ "national patriots' day 2019"
             ]
  , examples (datetimeHoliday (2013, 9, 2, 0, 0, 0) Day "Labour Day")
             [ "labor day"
             ]
  , examples (datetimeHoliday (2012, 9, 3, 0, 0, 0) Day "Labour Day")
             [ "labor day of last year"
             , "Labour Day 2012"
             ]
  , examples (datetimeIntervalHoliday ((2013, 8, 30, 18, 0, 0), (2013, 9, 3, 0, 0, 0)) Hour "Labour Day weekend")
             [ "labor day weekend"
             ]
  , examples (datetimeHoliday (2013, 2, 2, 0, 0, 0) Day "Groundhog Day")
             [ "Groundhog day"
             , "groundhogs day"
             ]
  , examples (datetimeHoliday (2010, 4, 21, 0, 0, 0) Day "Administrative Professionals' Day")
             [ "administrative professionals' day 2010"
             ]
  , examples (datetimeHoliday (2019, 4, 24, 0, 0, 0) Day "Administrative Professionals' Day")
             [ "admin day 2019"
             , "secretaries day in six years"
             ]
  ]
