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
  ]
