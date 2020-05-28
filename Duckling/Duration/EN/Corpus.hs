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
  [ examples (DurationData 1 Second)
             [ "one sec"
             , "1 second"
             , "1\""
             ]
  , examples (DurationData 2 Minute)
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
  , examples (DurationData 30 Day)
             [ "30 days"
             ]
  , examples (DurationData 7 Week)
             [ "seven weeks"
             ]
  , examples (DurationData 1 Month)
             [ "1 month"
             , "a month"
             ]
  , examples (DurationData 3 Quarter)
             [ "3 quarters"
             ]
  , examples (DurationData 2 Year)
             [ "2 years"
             ]
  , examples (DurationData 30 Minute)
             [ "half an hour"
             , "half hour"
             , "1/2 hour"
             , "1/2h"
             , "1/2 h"
             ]
  , examples (DurationData 12 Hour)
             [ "half a day"
             , "half day"
             , "1/2 day"
             ]
  , examples (DurationData 90 Minute)
             [ "an hour and a half"
             , "one hour and half"
             , "1 hour thirty"
             , "1 hour and thirty"
             , "1.5 hours"
             , "1.5 hrs"
             ]
  , examples (DurationData 75 Minute)
             [ "1 hour fifteen"
             , "1 hour and fifteen"
             ]
  , examples (DurationData 130 Minute)
             [ "2 hours ten"
             , "2 hour and 10"
             ]
  , examples (DurationData 3615 Second)
             [ "1 hour fifteen seconds"
             , "1 hour and fifteen seconds"
             ]
  , examples (DurationData 45 Day)
             [ "a month and a half"
             , "one month and half"
             ]
  , examples (DurationData 27 Month)
             [ "2 years and 3 months"
             , "2 years, 3 months"
             , "2 years 3 months"
             ]
  , examples (DurationData 31719604 Second)
             [ "1 year, 2 days, 3 hours and 4 seconds"
             , "1 year 2 days 3 hours and 4 seconds"
               -- Oxford comma not supported:
--           , "1 year, 2 days, 3 hours, and 4 seconds"
             ]
  , examples (DurationData 330 Second)
             [ "5 and a half minutes"
             , "five and half min"
             , "5 and an half minute"
             ]
  , examples (DurationData 105 Minute)
              [ "an hour and 45 minutes"
              , "one hour and 45 minutes"
              ]
  , examples (DurationData 90 Second)
              [ "a minute and 30 seconds"
              , "one minute and 30 seconds"
              ]
  , examples (DurationData 3630 Second)
              [ "an hour and 30 seconds"]
  ]
