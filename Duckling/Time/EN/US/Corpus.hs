-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.

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
  , examples (datetimeHoliday (2012, 11, 26, 0, 0, 0) Day "Cyber Monday")
             [ "last cyber monday"
             , "cyber monday 2012"
             ]
  , examples (datetimeHoliday (2017, 11, 27, 0, 0, 0) Day "Cyber Monday")
             [ "cyber monday 2017"
             ]
  , examples (datetimeHoliday (2013, 5, 27, 0, 0, 0) Day "Memorial Day")
             [ "memorial day"
             , "Next Memorial Day"
             , "decoration day"
             ]
  , examples (datetimeHoliday (2012, 5, 28, 0, 0, 0) Day "Memorial Day")
             [ "last memorial day"
             , "memorial day of last year"
             ]
  , examples (datetimeIntervalHoliday ((2013, 5, 24, 18, 0, 0), (2013, 5, 28, 0, 0, 0)) Hour "Memorial Day weekend")
             [ "memorial day week-end"
             ]
  , examples (datetimeHoliday (2013, 7, 4, 0, 0, 0) Day "Independence Day")
             [ "independence day"
             ]
  , examples (datetimeHoliday (2013, 11, 11, 0, 0, 0) Day "Veterans Day")
             [ "veterans day"
             , "veteran day"
             ]
  , examples (datetimeHoliday (2013, 5, 1, 0, 0, 0) Day "Loyalty Day")
             [ "law day"
             , "Lei Day"
             , "loyalty day"
             ]
  , examples (datetimeHoliday (2013, 2, 18, 0, 0, 0) Day "President's Day")
             [ "George Washington Day"
             , "washington's birthday"
             , "presidents' day"
             , "president day 2013"
             , "Daisy Gatson Bates Day"
             ]
  , examples (datetimeHoliday (2013, 2, 12, 0, 0, 0) Day "Lincoln's Birthday")
             [ "Lincolns birthday"
             , "Abraham Lincoln's birthday"
             , "Lincoln birthday"
             , "Lincolns' birthday"
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
  , examples (datetimeHoliday (2019, 9, 8, 0, 0, 0) Day "National Grandparents Day")
             [ "national grandparents day 2019"
             ]
  , examples (datetimeHoliday (2018, 5, 11, 0, 0, 0) Day "Military Spouse Day")
             [ "Military Spouse day 2018"
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
  , examples (datetimeHoliday (2013, 9, 2, 0, 0, 0) Day "Labor Day")
             [ "labor day"
             ]
  , examples (datetimeHoliday (2012, 9, 3, 0, 0, 0) Day "Labor Day")
             [ "labor day of last year"
             , "Labor Day 2012"
             ]
  , examples (datetimeIntervalHoliday ((2013, 8, 30, 18, 0, 0), (2013, 9, 3, 0, 0, 0)) Hour "Labor Day weekend")
             [ "labor day weekend"
             ]
  , examples (datetimeHoliday (2013, 2, 2, 0, 0, 0) Day "Groundhog Day")
             [ "Groundhog day"
             , "groundhogs day"
             ]
  , examples (datetimeHoliday (2011, 4, 15, 0, 0, 0) Day "Emancipation Day")
             [ "emancipation day 2011"
             ]
  , examples (datetimeHoliday (2012, 4, 16, 0, 0, 0) Day "Emancipation Day")
             [ "emancipation day 2012"
             ]
  , examples (datetimeHoliday (2017, 4, 17, 0, 0, 0) Day "Emancipation Day")
             [ "emancipation day 2017"
             ]
  , examples (datetimeHoliday (2011, 4, 18, 0, 0, 0) Day "Tax Day")
             [ "tax day 2011"
             , "tax day two years ago"
             ]
  , examples (datetimeHoliday (2012, 4, 17, 0, 0, 0) Day "Tax Day")
             [ "tax day 2012"
             , "tax day last year"
             ]
  , examples (datetimeHoliday (2013, 4, 15, 0, 0, 0) Day "Tax Day")
             [ "next tax day"
             , "tax day 2013"
             ]
  , examples (datetimeHoliday (2017, 4, 18, 0, 0, 0) Day "Tax Day")
             [ "tax day 2017"
             , "tax day in 4 years"
             ]
  , examples (datetimeHoliday (2013, 4, 10, 0, 0, 0) Day "Siblings Day")
             [ "siblings day"
             , "national sibling day"
             ]
  , examples (datetimeHoliday (2016, 4, 27, 0, 0, 0) Day "Administrative Professionals' Day")
             [ "administrative professionals' day 2016"
             , "administrative professionals day 2016"
             , "administrative professional day 2016"
             , "administrative professional's day of 2016"
             , "secretaries' day 2016"
             , "admins day 2016"
             ]
  , examples (datetimeHoliday (2010, 4, 21, 0, 0, 0) Day "Administrative Professionals' Day")
             [ "administrative professionals' day 2010"
             ]
  , examples (datetimeHoliday (2019, 4, 24, 0, 0, 0) Day "Administrative Professionals' Day")
             [ "administrative professionals' day 2019"
             ]
  , examples (datetimeIntervalHoliday ((2013, 4, 26, 0, 0, 0), (2013, 4, 29, 0, 0, 0)) Day "Global Youth Service Day")
             [ "national youth service day"
             ]
  , examples (datetimeIntervalHoliday ((2018, 5, 20, 0, 0, 0), (2018, 5, 27, 0, 0, 0)) Day "Emergency Medical Services Week")
             [ "ems week 2018"
             ]
  , examples (datetimeHoliday (2018, 5, 23, 0, 0, 0) Day "Emergency Medical Services for Children Day")
             [ "emsc day 2018"
             ]
  , examples (datetimeHoliday (2017, 3, 12, 0, 0, 0) Day "Daylight Saving Start Day")
             [ "daylight saving start day 2017"
             , "daylight saving start 2017"
             , "daylight savings start day 2017"
             , "daylight savings start 2017"
             ]
  , examples (datetimeHoliday (2017, 11, 5, 0, 0, 0) Day "Daylight Saving End Day")
             [ "daylight saving end day 2017"
             , "daylight saving end 2017"
             , "daylight savings end day 2017"
             , "daylight savings end 2017"
             ]
  ]
