-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.

{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.EN.AU.Corpus
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
             [ "15/2"
             , "on 15/2"
             , "15 / 2"
             , "15-2"
             , "15 - 2"
             ]
  , examples (datetime (1974, 10, 31, 0, 0, 0) Day)
             [ "31/10/1974"
             , "31/10/74"
             , "31-10-74"
             , "31.10.1974"
             , "31 10 1974"
             ]
  , examples (datetime (2013, 4, 25, 16, 0, 0) Minute)
             [ "25/4 at 4:00pm"
             ]
  , examples (datetime (2013, 10, 10, 0, 0, 0) Day)
             [ "10/10"
             , "10/10/2013"
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
  , examples (datetimeHoliday (2013, 4, 25, 0, 0, 0) Day "ANZAC Day")
             [ "anzac day"
             ]
  , examples (datetimeHoliday (2013, 9, 1, 0, 0, 0) Day "Father's Day")
             [ "Father's Day"
             ]
  , examples (datetimeHoliday (2012, 9, 2, 0, 0, 0) Day "Father's Day")
             [ "last fathers day"
             ]
  , examples (datetimeHoliday (1996, 9, 1, 0, 0, 0) Day "Father's Day")
             [ "fathers day 1996"
             ]
  , examples (datetimeIntervalHoliday ((2020, 2, 8, 0, 0, 0), (2020, 2, 11, 0, 0, 0)) Day "Royal Hobart Regatta")
             [ "Royal Hobart Regatta 2020"
             ]
  , examples (datetimeIntervalHoliday ((2018, 7, 8, 0, 0, 0), (2018, 7, 16, 0, 0, 0)) Day "NAIDOC Week")
             [ "NAIDOC week 2018"
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
  , examples (datetimeHoliday (2013, 10, 7, 0, 0, 0) Day "Labour Day")
             [ "labour day"
             ]
  , examples (datetimeHoliday (2012, 10, 1, 0, 0, 0) Day "Labour Day")
             [ "labour day of last year"
             , "Labour Day 2012"
             ]
  , examples (datetimeHoliday (2018, 5, 28, 0, 0, 0) Day "Reconciliation Day")
             [ "reconciliation day 2018"
             ]
  , examples (datetimeHoliday (2019, 5, 27, 0, 0, 0) Day "Reconciliation Day")
             [ "reconciliation day 2019"
             ]
  , examples (datetimeIntervalHoliday ((2013, 8, 9, 0, 0, 0), (2013, 8, 19, 0, 0, 0)) Day "Royal Queensland Show")
             [ "ekka"
             , "royal national agricultural show"
             ]
  , examples (datetimeHoliday (2018, 8, 15, 0, 0, 0) Day "Royal Queensland Show Day")
             [ "ekka day 2018"
             , "RNA Show Day 2018"
             , "Royal Queensland Show Day in five years"
             ]
  , examples (datetimeHoliday (2013, 5, 3, 0, 0, 0) Day "Administrative Professionals' Day")
             [ "admin day"
             ]
  ]
