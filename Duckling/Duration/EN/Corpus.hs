-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.EN.Corpus
  ( corpus
  , negativeCorpus
  ) where

import Prelude
import Data.String

import Duckling.Duration.Types
import Duckling.Testing.Types
import Duckling.TimeGrain.Types (Grain(..))

corpus :: Corpus
corpus = (testContext, testOptions, allExamples)

negativeCorpus :: NegativeCorpus
negativeCorpus = (testContext, testOptions, examples)
  where
    examples =
      [ "for months"
      , "in days"
      , "secretary"
      , "minutes"
      , "I second that"
      ]

allExamples :: [Example]
allExamples = concat
  [ examples (single 1 Second)
             [ "one sec"
             , "1 second"
             , "1\""
             ]
  , examples (single 2 Minute)
             [ "2 mins"
             , "two minutes"
             , "2'"
             , "2 more minutes"
             , "two additional minutes"
             , "2 extra minutes"
             , "2 less minutes"
             , "2 fewer minutes"
             , "2m"
             , "2 m"
             ]
  , examples (single 30 Day)
             [ "30 days"
             ]
  , examples (single 7 Week)
             [ "seven weeks"
             ]
  , examples (single 1 Month)
             [ "1 month"
             , "a month"
             ]
  , examples (single 3 Quarter)
             [ "3 quarters"
             ]
  , examples (single 2 Year)
             [ "2 years"
             ]
  , examples (single 30 Minute)
             [ "half an hour"
             , "half hour"
             , "1/2 hour"
             , "1/2h"
             , "1/2 h"
             ]
  , examples (single 12 Hour)
             [ "half a day"
             , "half day"
             , "1/2 day"
             ]
  , examples (single 90 Minute)
             [ "an hour and a half"
             , "one hour and half"
             , "1 hour thirty"
             , "1 hour and thirty"
             , "1.5 hours"
             , "1.5 hrs"
             , "one and two quarter hour"
             , "one and two quarters hour"
             , "one and two quarter of hour"
             , "one and two quarters of hour"
             ]
  , examples (single 75 Minute)
             [ "1 hour fifteen"
             , "1 hour and fifteen"
             , "one and quarter hour"
             , "one and a quarter hour"
             , "one and one quarter hour"
             , "one and quarter of hour"
             , "one and a quarter of hour"
             , "one and one quarter of hour"
             ]
  , examples (single 130 Minute)
             [ "2 hours ten"
             , "2 hour and 10"
             ]
  , examples (single 3615 Second)
             [ "1 hour fifteen seconds"
             , "1 hour and fifteen seconds"
             ]
  , examples (single 45 Day)
             [ "a month and a half"
             , "one month and half"
             ]
  , examples (single 27 Month)
             [ "2 years and 3 months"
             , "2 years, 3 months"
             , "2 years 3 months"
             ]
  , examples (single 31719604 Second)
             [ "1 year, 2 days, 3 hours and 4 seconds"
             , "1 year 2 days 3 hours and 4 seconds"
               -- Oxford comma not supported:
--           , "1 year, 2 days, 3 hours, and 4 seconds"
             ]
  , examples (single 330 Second)
             [ "5 and a half minutes"
             , "five and half min"
             , "5 and an half minute"
             ]
  , examples (single 105 Minute)
              [ "one and three quarter hour"
              , "one and three quarters hour"
              , "one and three quarter of hour"
              , "one and three quarters of hour"
              , "one and three quarter of hours"
              , "one and three quarters of hours"
              ]
  , examples (single 135 Minute)
             [ "two and quarter hour"
             , "two and a quarter of hour"
             , "two and quarter of hours"
             , "two and a quarter of hours"
             ]
  , examples (single 105 Minute)
              [ "an hour and 45 minutes"
              , "one hour and 45 minutes"
              ]
  , examples (single 90 Second)
              [ "a minute and 30 seconds"
              , "one minute and 30 seconds"
              ]
  , examples (single 3630 Second)
              [ "an hour and 30 seconds"]
  , examples (single 930 Second)
              [ "15.5 minutes"
              , "15.5 minute"
              , "15.5 mins"
              , "15.5 min"
              ]
  , examples (between (60,90) Day)
             [ "between 60 days and 90 days"
             , "from sixty to ninety days"
             , "around 60-90 days"
             , "about 60-90 days"
             , "60-90 days"
             ]
  , examples (under 5 Second)
             [ "under 5 seconds"
             , "less than 5 secs"
             , "lower than five seconds"
             ]
  , examples (above 3000 Year)
             [ "more than three thousand years"
             , "at least 3000 years"
             , "over 3K years"
             , "above 3 thousand years"
             ]
  ]
