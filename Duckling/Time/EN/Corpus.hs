-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.EN.Corpus
  ( corpus
  , defaultCorpus
  , negativeCorpus
  , latentCorpus
  ) where

import Data.String
import Prelude

import Duckling.Resolve
import Duckling.Testing.Types hiding (examples)
import Duckling.Time.Corpus
import Duckling.Time.Types hiding (Month)
import Duckling.TimeGrain.Types hiding (add)

corpus :: Corpus
corpus = (testContext, testOptions, allExamples)

defaultCorpus :: Corpus
defaultCorpus = (testContext, testOptions, allExamples ++ custom)
  where
    custom = concat
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
      ]

negativeCorpus :: NegativeCorpus
negativeCorpus = (testContext, testOptions, examples)
  where
    examples =
      [ "laughing out loud"
      , "1 adult"
      , "we are separated"
      , "25"
      , "this is the one"
      , "in 61"
      , "this one"
      , "this past one"
      , "at single"
      , "at a couple of"
      , "at pairs"
      , "at a few"
      , "at dozens"
      , "single o'clock"
      , "dozens o'clock"
      , "Rat 6"
      , "rat 6"
      , "3 30"
      , "three twenty"
      , "at 650.650.6500"
      , "at 650-650-6500"
      , "two sixty a m"
      ]

latentCorpus :: Corpus
latentCorpus = (testContext, testOptions {withLatent = True}, xs)
  where
    xs = concat
      [ examples (datetime (2013, 2, 24, 0, 0, 0) Day)
                 [ "the 24"
                 , "On 24th"
                 ]
      , examples (datetime (2013, 2, 12, 7, 0, 0) Hour)
                 [ "7"
                 ]
      , examples (datetimeInterval
          ((2013, 2, 12, 4, 0, 0), (2013, 2, 12, 12, 0, 0)) Hour)
                 [ "morning"
                 ]
      , examples (datetimeInterval
          ((2013, 2, 12, 12, 0, 0), (2013, 2, 12, 19, 0, 0)) Hour)
                 [ "afternoon"
                 ]
      , examples (datetimeInterval
          ((2013, 2, 12, 18, 0, 0), (2013, 2, 13, 0, 0, 0)) Hour)
                 [ "evening"
                 ]
      , examples (datetimeInterval
          ((2013, 2, 12, 18, 0, 0), (2013, 2, 13, 0, 0, 0)) Hour)
                 [ "night"
                 ]
      ]

allExamples :: [Example]
allExamples = concat
  [ examples (datetime (2013, 2, 12, 4, 30, 0) Second)
             [ "now"
             , "right now"
             , "just now"
             ]
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "today"
             , "at this time"
             ]
  , examples (datetime (2013, 2, 1, 0, 0, 0) Day)
             [ "2/2013"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             [ "yesterday"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "tomorrow"
             , "tomorrows"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "monday"
             , "mon."
             , "this monday"
             , "Monday, Feb 18"
             , "Mon, February 18"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "tuesday"
             , "Tuesday the 19th"
             , "Tuesday 19th"
             ]
  , examples (datetime (2013, 8, 15, 0, 0, 0) Day)
             [ "Thu 15th"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "thursday"
             , "thu"
             , "thu."
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "friday"
             , "fri"
             , "fri."
             ]
  , examples (datetime (2013, 2, 16, 0, 0, 0) Day)
             [ "saturday"
             , "sat"
             , "sat."
             ]
  , examples (datetime (2013, 2, 17, 0, 0, 0) Day)
             [ "sunday"
             , "sun"
             , "sun."
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Day)
             [ "the 1st of march"
             , "first of march"
             , "march first"
             ]
  , examples (datetime (2013, 3, 3, 0, 0, 0) Day)
             [ "march 3"
             ]
  , examples (datetime (2013, 3, 15, 0, 0, 0) Day)
             [ "the ides of march"
             ]
  , examples (datetime (2015, 3, 3, 0, 0, 0) Day)
             [ "march 3 2015"
             , "march 3rd 2015"
             , "march third 2015"
             , "3/3/2015"
             , "3/3/15"
             , "2015-3-3"
             , "2015-03-03"
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "on the 15th"
             , "the 15th of february"
             , "15 of february"
             , "february the 15th"
             , "february 15"
             , "15th february"
             , "February 15"
             ]
  , examples (datetime (2013, 8, 8, 0, 0, 0) Day)
             [ "Aug 8"
             ]
  , examples (datetime (2014, 7, 18, 0, 0, 0) Day)
             [ "Fri, Jul 18"
             , "Jul 18, Fri"
             ]
  , examples (datetime (2014, 10, 1, 0, 0, 0) Month)
             [ "October 2014"
             ]
  , examples (datetime (2015, 4, 14, 0, 0, 0) Day)
             [ "14april 2015"
             , "April 14, 2015"
             , "14th April 15"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "next tuesday"
             , "around next tuesday"
             ]
  , examples (datetime (2013, 2, 22, 0, 0, 0) Day)
             [ "friday after next"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Month)
             [ "next March"
             ]
  , examples (datetime (2014, 3, 1, 0, 0, 0) Month)
             [ "March after next"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "Sunday, Feb 10"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "Wed, Feb13"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Week)
             [ "this week"
             , "current week"
             , "coming week"
             ]
  , examples (datetime (2013, 2, 4, 0, 0, 0) Week)
             [ "last week"
             , "past week"
             , "previous week"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Week)
             [ "next week"
             , "the following week"
             , "around next week"
             ]
  , examples (datetime (2013, 1, 1, 0, 0, 0) Month)
             [ "last month"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Month)
             [ "next month"
             ]
  , examples (datetime (2013, 1, 1, 0, 0, 0) Quarter)
             [ "this quarter"
             , "this qtr"
             ]
  , examples (datetime (2013, 4, 1, 0, 0, 0) Quarter)
             [ "next quarter"
             , "next qtr"
             ]
  , examples (datetime (2013, 7, 1, 0, 0, 0) Quarter)
             [ "third quarter"
             , "3rd quarter"
             , "third qtr"
             , "3rd qtr"
             , "the 3rd qtr"
             ]
  , examples (datetime (2018, 10, 1, 0, 0, 0) Quarter)
             [ "4th quarter 2018"
             , "4th qtr 2018"
             , "the 4th qtr of 2018"
             ]
  , examples (datetime (2012, 1, 1, 0, 0, 0) Year)
             [ "last year"
             , "last yr"
             ]
  , examples (datetime (2013, 1, 1, 0, 0, 0) Year)
             [ "this year"
             , "current year"
             , "this yr"
             ]
  , examples (datetime (2014, 1, 1, 0, 0, 0) Year)
             [ "next year"
             , "next yr"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "last sunday"
             , "sunday from last week"
             , "last week's sunday"
             ]
  , examples (datetime (2013, 2, 5, 0, 0, 0) Day)
             [ "last tuesday"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "next tuesday"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "next wednesday"
             ]
  , examples (datetime (2013, 2, 20, 0, 0, 0) Day)
             [ "wednesday of next week"
             , "wednesday next week"
             , "wednesday after next"
             ]
  , examples (datetime (2013, 2, 22, 0, 0, 0) Day)
             [ "friday after next"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             [ "monday of this week"
             ]
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "tuesday of this week"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "wednesday of this week"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "the day after tomorrow"
             ]
  , examples (datetime (2013, 2, 14, 17, 0, 0) Hour)
             [ "day after tomorrow 5pm"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "the day before yesterday"
             ]
  , examples (datetime (2013, 2, 10, 8, 0, 0) Hour)
             [ "day before yesterday 8am"
             ]
  , examples (datetime (2013, 3, 25, 0, 0, 0) Day)
             [ "last Monday of March"
             ]
  , examples (datetime (2014, 3, 30, 0, 0, 0) Day)
             [ "last Sunday of March 2014"
             ]
  , examples (datetime (2013, 10, 3, 0, 0, 0) Day)
             [ "third day of october"
             ]
  , examples (datetime (2014, 10, 6, 0, 0, 0) Week)
             [ "first week of october 2014"
             ]
  , examples (datetime (2013, 10, 7, 0, 0, 0) Week)
             [ "the week of october 6th"
             ]
  , examples (datetime (2013, 10, 7, 0, 0, 0) Week)
             [ "the week of october 7th"
             ]
  , examples (datetime (2015, 10, 31, 0, 0, 0) Day)
             [ "last day of october 2015"
             , "last day in october 2015"
             ]
  , examples (datetime (2014, 9, 22, 0, 0, 0) Week)
             [ "last week of september 2014"
             ]
  , examples (datetime (2013, 10, 1, 0, 0, 0) Day)
             [ "first tuesday of october"
             , "first tuesday in october"
             ]
  , examples (datetime (2014, 9, 16, 0, 0, 0) Day)
             [ "third tuesday of september 2014"
             ]
  , examples (datetime (2014, 10, 1, 0, 0, 0) Day)
             [ "first wednesday of october 2014"
             ]
  , examples (datetime (2014, 10, 8, 0, 0, 0) Day)
             [ "second wednesday of october 2014"
             ]
  , examples (datetime (2015, 1, 13, 0, 0, 0) Day)
             [ "third tuesday after christmas 2014"
             ]
  , examples (datetime (2013, 2, 13, 3, 0, 0) Hour)
             [ "at 3am"
             , "3 in the AM"
             , "at 3 AM"
             , "3 oclock am"
             , "at three am"
             ]
  , examples (datetime (2013, 2, 13, 3, 18, 0) Minute)
             [ "3:18am"
             , "3:18a"
             ]
  , examples (datetime (2013, 2, 12, 15, 0, 0) Hour)
             [ "at 3pm"
             , "@ 3pm"
             , "3PM"
             , "3pm"
             , "3 oclock pm"
             , "3 o'clock in the afternoon"
             , "3ish pm"
             , "3pm approximately"
             , "at about 3pm"
             ]
  , examples (datetime (2013, 2, 12, 15, 15, 0) Minute)
             [ "at 15 past 3pm"
             , "a quarter past 3pm"
             , "3:15 in the afternoon"
             , "15:15"
             , "3:15pm"
             , "3:15PM"
             , "3:15p"
             , "at 3 15"
             ]
  , examples (datetime (2013, 2, 12, 15, 20, 0) Minute)
             [ "at 20 past 3pm"
             , "3:20 in the afternoon"
             , "3:20 in afternoon"
             , "twenty after 3pm"
             , "3:20p"
             , "at three twenty"
             ]
  , examples (datetime (2013, 2, 12, 15, 30, 0) Minute)
             [ "at half past three pm"
             , "half past 3 pm"
             , "15:30"
             , "3:30pm"
             , "3:30PM"
             , "330 p.m."
             , "3:30 p m"
             , "3:30"
             , "half three"
             ]
  , examples (datetime (2013, 2, 12, 9, 59, 0) Minute)
             [ "nine fifty nine a m"
             ]
  , examples (datetime (2013, 2, 12, 15, 23, 24) Second)
             [ "15:23:24"
             ]
  , examples (datetime (2013, 2, 12, 11, 45, 0) Minute)
             [ "a quarter to noon"
             , "11:45am"
             , "15 to noon"
             ]
  , examples (datetime (2013, 2, 12, 20, 0, 0) Hour)
             [ "8 tonight"
             , "eight tonight"
             , "8 this evening"
             , "at 8 in the evening"
             , "in the evening at eight"
             ]
  , examples (datetime (2013, 9, 20, 19, 30, 0) Minute)
             [ "at 7:30 PM on Fri, Sep 20"
             ]
  , examples (datetime (2013, 2, 16, 9, 0, 0) Hour)
             [ "at 9am on Saturday"
             ]
  , examples (datetime (2013, 2, 16, 9, 0, 0) Hour)
             [ "on Saturday for 9am"
             ]
  , examples (datetime (2014, 7, 18, 19, 0, 0) Minute)
             [ "Fri, Jul 18, 2014 07:00 PM"
             ]
  , examples (datetime (2013, 2, 12, 4, 30, 1) Second)
             [ "in a sec"
             , "one second from now"
             , "in 1\""
             ]
  , examples (datetime (2013, 2, 12, 4, 31, 0) Second)
             [ "in a minute"
             , "in one minute"
             , "in 1'"
             ]
  , examples (datetime (2013, 2, 12, 4, 32, 0) Second)
             [ "in 2 minutes"
             , "in 2 more minutes"
             , "2 minutes from now"
             , "in a couple of minutes"
             , "in a pair of minutes"
             ]
  , examples (datetime (2013, 2, 12, 4, 33, 0) Second)
             [ "in three minutes"
             , "in a few minutes"
             ]
  , examples (datetime (2013, 2, 12, 5, 30, 0) Second)
             [ "in 60 minutes"
             ]
  , examples (datetime (2013, 2, 12, 4, 45, 0) Second)
             [ "in a quarter of an hour"
             , "in 1/4h"
             , "in 1/4 h"
             , "in 1/4 hour"
             ]
  , examples (datetime (2013, 2, 12, 5, 0, 0) Second)
             [ "in half an hour"
             , "in 1/2h"
             , "in 1/2 h"
             , "in 1/2 hour"
             ]
  , examples (datetime (2013, 2, 12, 5, 15, 0) Second)
             [ "in three-quarters of an hour"
             , "in 3/4h"
             , "in 3/4 h"
             , "in 3/4 hour"
             ]
  , examples (datetime (2013, 2, 12, 7, 0, 0) Second)
             [ "in 2.5 hours"
             , "in 2 and an half hours"
             ]
  , examples (datetime (2013, 2, 12, 5, 30, 0) Minute)
             [ "in one hour"
             , "in 1h"
             ]
  , examples (datetime (2013, 2, 12, 6, 30, 0) Minute)
             [ "in a couple hours"
             , "in a couple of hours"
             ]
  , examples (datetime (2013, 2, 12, 7, 30, 0) Minute)
             [ "in a few hours"
             , "in few hours"
             ]
  , examples (datetime (2013, 2, 13, 4, 30, 0) Minute)
             [ "in 24 hours"
             ]
  , examples (datetime (2013, 2, 13, 4, 0, 0) Hour)
             [ "in a day"
             , "a day from now"
             ]
  , examples (datetime (2013, 2, 13, 4, 30, 0) Second)
             [ "a day from right now"
             ]
  , examples (datetime (2016, 2, 12, 0, 0, 0) Day)
             [ "3 years from today"
             ]
  , examples (datetime (2013, 2, 19, 4, 0, 0) Hour)
             [ "in 7 days"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "in 1 week"
             , "in a week"
             ]
  , examples (datetime (2013, 2, 12, 5, 0, 0) Second)
             [ "in about half an hour"
             ]
  , examples (datetime (2013, 2, 5, 4, 0, 0) Hour)
             [ "7 days ago"
             ]
  , examples (datetime (2013, 1, 29, 4, 0, 0) Hour)
             [ "14 days Ago"
             , "a fortnight ago"
             ]
  , examples (datetime (2013, 2, 5, 0, 0, 0) Day)
             [ "a week ago"
             , "one week ago"
             , "1 week ago"
             ]
  , examples (datetime (2013, 1, 22, 0, 0, 0) Day)
             [ "three weeks ago"
             ]
  , examples (datetime (2012, 11, 12, 0, 0, 0) Day)
             [ "three months ago"
             ]
  , examples (datetime (2011, 2, 1, 0, 0, 0) Month)
             [ "two years ago"
             ]
  , examples (datetime (1954, 1, 1, 0, 0, 0) Year)
             [ "1954"
             ]
  , examples (datetime (2013, 2, 19, 4, 0, 0) Hour)
             [ "7 days hence"
             ]
  , examples (datetime (2013, 2, 26, 4, 0, 0) Hour)
             [ "14 days hence"
             , "a fortnight hence"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "a week hence"
             , "one week hence"
             , "1 week hence"
             ]
  , examples (datetime (2013, 3, 5, 0, 0, 0) Day)
             [ "three weeks hence"
             ]
  , examples (datetime (2013, 5, 12, 0, 0, 0) Day)
             [ "three months hence"
             ]
  , examples (datetime (2015, 2, 1, 0, 0, 0) Month)
             [ "two years hence"
             ]
  , examples (datetime (2013, 12, 25, 0, 0, 0) Day)
             [ "one year After christmas"
             , "a year from Christmas"
             ]
  , examples (datetimeInterval ((2013, 12, 18, 0, 0, 0), (2013, 12, 29, 0, 0, 0)) Day)
             [ "for 10 days from 18th Dec"
             ]
  , examples (datetimeInterval ((2013, 6, 21, 0, 0, 0), (2013, 9, 24, 0, 0, 0)) Day)
             [ "this Summer"
             , "current summer"
             ]
  , examples (datetimeInterval ((2012, 12, 21, 0, 0, 0), (2013, 3, 21, 0, 0, 0)) Day)
             [ "this winter"
             ]
  , examples (datetime (2013, 12, 25, 0, 0, 0) Day)
             [ "xmas"
             , "christmas"
             , "christmas day"
             ]
  , examples (datetime (2013, 12, 31, 0, 0, 0) Day)
             [ "new year's eve"
             , "new years eve"
             ]
  , examples (datetime (2014, 1, 1, 0, 0, 0) Day)
             [ "new year's day"
             , "new years day"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "valentine's day"
             , "valentine day"
             ]
  , examples (datetime (2013, 5, 12, 0, 0, 0) Day)
             [ "Mother's Day"
             , "next mothers day"
             ]
  , examples (datetime (2012, 5, 13, 0, 0, 0) Day)
             [ "last mothers day"
             ]
  , examples (datetime (2014, 5, 11, 0, 0, 0) Day)
             [ "mothers day 2014"
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
  , examples (datetime (2013, 7, 4, 0, 0, 0) Day)
             [ "4th of July"
             , "4 of july"
             ]
  , examples (datetime (2013, 9, 2, 0, 0, 0) Day)
             [ "labor day"
             ]
  , examples (datetime (2012, 9, 3, 0, 0, 0) Day)
             [ "labor day of last year"
             , "Labor Day 2012"
             ]
  , examples (datetimeInterval ((2013, 8, 30, 18, 0, 0), (2013, 9, 3, 0, 0, 0)) Hour)
             [ "labor day weekend"
             ]
  , examples (datetime (2013, 10, 31, 0, 0, 0) Day)
             [ "halloween"
             , "next halloween"
             , "Halloween 2013"
             ]
  , examples (datetime (2013, 11, 29, 0, 0, 0) Day)
             [ "black friday"
             , "black friday of this year"
             , "black friday 2013"
             ]
  , examples (datetime (2017, 11, 24, 0, 0, 0) Day)
             [ "black friday 2017"
             ]
  , examples (datetime (2014, 1, 20, 0, 0, 0) Day)
             [ "MLK day"
             , "next Martin Luther King day"
             , "this MLK day"
             ]
  , examples (datetime (2013, 1, 21, 0, 0, 0) Day)
             [ "last MLK Jr. day"
             , "MLK day 2013"
             ]
  , examples (datetime (2012, 1, 16, 0, 0, 0) Day)
             [ "MLK day of last year"
             , "MLK day 2012"
             ]
  , examples (datetime (2013, 11, 1, 0, 0, 0) Day)
             [ "world vegan day"
             ]
  , examples (datetime (2013, 3, 31, 0, 0, 0) Day)
             [ "easter"
             , "easter 2013"
             ]
  , examples (datetime (2013, 4, 1, 0, 0, 0) Day)
             [ "easter mon"
             ]
  , examples (datetime (2010, 4, 4, 0, 0, 0) Day)
             [ "easter 2010"
             , "Easter Sunday two thousand ten"
             ]
  , examples (datetime (2013, 4, 3, 0, 0, 0) Day)
             [ "three days after Easter"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 18, 0, 0), (2013, 2, 13, 0, 0, 0)) Hour)
             [ "this evening"
             , "today evening"
             , "tonight"
             ]
  , examples (datetimeInterval ((2013, 2, 8, 18, 0, 0), (2013, 2, 11, 0, 0, 0)) Hour)
             [ "this past weekend"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 18, 0, 0), (2013, 2, 14, 0, 0, 0)) Hour)
             [ "tomorrow evening"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 12, 0, 0), (2013, 2, 13, 14, 0, 0)) Hour)
             [ "tomorrow lunch"
             , "tomorrow at lunch"
             ]
  , examples (datetimeInterval ((2013, 2, 11, 18, 0, 0), (2013, 2, 12, 0, 0, 0)) Hour)
             [ "yesterday evening"
             ]
  , examples (datetimeInterval ((2013, 2, 15, 18, 0, 0), (2013, 2, 18, 0, 0, 0)) Hour)
             [ "this week-end"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 4, 0, 0), (2013, 2, 18, 12, 0, 0)) Hour)
             [ "monday mOrnIng"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 4, 0, 0), (2013, 2, 18, 9, 0, 0)) Hour)
             [ "monday early in the morning"
             , "monday early morning"
             , "monday in the early hours of the morning"
             ]
  , examples (datetimeInterval ((2013, 2, 15, 4, 0, 0), (2013, 2, 15, 12, 0, 0)) Hour)
             [ "february the 15th in the morning"
             , "15 of february in the morning"
             , "morning of the 15th of february"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 29, 58), (2013, 2, 12, 4, 30, 0)) Second)
             [ "last 2 seconds"
             , "last two seconds"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 1), (2013, 2, 12, 4, 30, 4)) Second)
             [ "next 3 seconds"
             , "next three seconds"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 28, 0), (2013, 2, 12, 4, 30, 0)) Minute)
             [ "last 2 minutes"
             , "last two minutes"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 31, 0), (2013, 2, 12, 4, 34, 0)) Minute)
             [ "next 3 minutes"
             , "next three minutes"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 3, 0, 0), (2013, 2, 12, 4, 0, 0)) Hour)
             [ "last 1 hour"
             , "last one hour"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 5, 0, 0), (2013, 2, 12, 8, 0, 0)) Hour)
             [ "next 3 hours"
             , "next three hours"
             ]
  , examples (datetimeInterval ((2013, 2, 10, 0, 0, 0), (2013, 2, 12, 0, 0, 0)) Day)
             [ "last 2 days"
             , "last two days"
             , "past 2 days"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 0, 0, 0), (2013, 2, 16, 0, 0, 0)) Day)
             [ "next 3 days"
             , "next three days"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 0, 0, 0), (2013, 2, 16, 0, 0, 0)) Day)
             [ "next few days"
             ]
  , examples (datetimeInterval ((2013, 1, 28, 0, 0, 0), (2013, 2, 11, 0, 0, 0)) Week)
             [ "last 2 weeks"
             , "last two weeks"
             , "past 2 weeks"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 0, 0, 0), (2013, 3, 11, 0, 0, 0)) Week)
             [ "next 3 weeks"
             , "next three weeks"
             ]
  , examples (datetimeInterval ((2012, 12, 1, 0, 0, 0), (2013, 2, 1, 0, 0, 0)) Month)
             [ "last 2 months"
             , "last two months"
             ]
  , examples (datetimeInterval ((2013, 3, 1, 0, 0, 0), (2013, 6, 1, 0, 0, 0)) Month)
             [ "next 3 months"
             , "next three months"
             ]
  , examples (datetimeInterval ((2011, 1, 1, 0, 0, 0), (2013, 1, 1, 0, 0, 0)) Year)
             [ "last 2 years"
             , "last two years"
             ]
  , examples (datetimeInterval ((2014, 1, 1, 0, 0, 0), (2017, 1, 1, 0, 0, 0)) Year)
             [ "next 3 years"
             , "next three years"
             ]
  , examples (datetimeInterval ((2013, 7, 13, 0, 0, 0), (2013, 7, 16, 0, 0, 0)) Day)
             [ "July 13-15"
             , "July 13 to 15"
             , "July 13 thru 15"
             , "July 13 through 15"
             , "July 13 - July 15"
             , "from July 13-15"
             , "from 13 to 15 July"
             , "from 13th to 15th July"
             ]
  , examples (datetimeInterval ((2013, 8, 8, 0, 0, 0), (2013, 8, 13, 0, 0, 0)) Day)
             [ "Aug 8 - Aug 12"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 9, 30, 0), (2013, 2, 12, 11, 1, 0)) Minute)
             [ "9:30 - 11:00"
             ]
  , examples (datetimeInterval ((2013, 2, 14, 9, 30, 0), (2013, 2, 14, 11, 1, 0)) Minute)
             [ "from 9:30 - 11:00 on Thursday"
             , "between 9:30 and 11:00 on thursday"
             , "between 9:30 and 11:00 on thursday"
             , "between 9:30 and 11:00 on thursday"
             , "9:30 - 11:00 on Thursday"
             , "later than 9:30 but before 11:00 on Thursday"
             , "Thursday from 9:30 to 11:00"
             , "from 9:30 untill 11:00 on thursday"
             , "Thursday from 9:30 untill 11:00"
             , "9:30 till 11:00 on Thursday"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 1, 0, 0), (2013, 2, 13, 2, 31, 0)) Minute)
             [ "tomorrow in between 1-2:30 ish"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 15, 0, 0), (2013, 2, 12, 17, 0, 0)) Hour)
             [ "3-4pm"
             , "from 3 to 4 in the PM"
             , "around 3-4pm"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 15, 30, 0), (2013, 2, 12, 19, 0, 0)) Minute)
             [ "3:30 to 6 PM"
             , "3:30-6 p.m."
             ]
  , examples (datetimeInterval ((2013, 2, 12, 8, 0, 0), (2013, 2, 12, 14, 0, 0)) Hour)
             [ "8am - 1pm"
             ]
  , examples (datetimeInterval ((2013, 2, 14, 9, 0, 0), (2013, 2, 14, 12, 0, 0)) Hour)
             [ "Thursday from 9a to 11a"
             , "this Thu 9-11am"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 11, 30, 0), (2013, 2, 12, 13, 31, 0)) Minute)
             [ "11:30-1:30"
             ]
  , examples (datetime (2013, 9, 21, 13, 30, 0) Minute)
             [ "1:30 PM on Sat, Sep 21"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 0), (2013, 2, 26, 0, 0, 0)) Second)
             [ "Within 2 weeks"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 0), (2013, 2, 12, 14, 0, 0)) Second)
             [ "by 2:00pm"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 0), (2013, 2, 13, 0, 0, 0)) Second)
             [ "by EOD"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 0), (2013, 3, 1, 0, 0, 0)) Second)
             [ "by EOM"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 0), (2013, 4, 1, 0, 0, 0)) Second)
             [ "by the end of next month"
             ]
  , examples (datetime (2013, 2, 12, 13, 0, 0) Minute)
             [ "4pm CET"
             ]
  , examples (datetime (2013, 2, 14, 6, 0, 0) Minute)
             [ "Thursday 8:00 GMT"
             , "Thursday 8:00 gmt"
             , "Thu at 8 GMT"
             , "Thu at 8 gmt"
             ]
  , examples (datetime (2013, 2, 14, 14, 0, 0) Minute)
             [ "Thursday 8:00 PST"
             , "Thursday 8:00 pst"
             , "Thu at 8 am PST"
             , "Thu at 8 am pst"
             ]
  , examples (datetime (2013, 2, 12, 14, 0, 0) Hour)
             [ "today at 2pm"
             , "at 2pm"
             ]
  , examples (datetime (2013, 2, 13, 15, 0, 0) Hour)
             [ "3pm tomorrow"
             ]
  , examples (datetimeOpenInterval Before (2013, 2, 12, 14, 0, 0) Minute)
             [ "until 2:00pm"
             , "through 2:00pm"
             ]
  , examples (datetimeOpenInterval After (2013, 2, 12, 14, 0, 0) Hour)
             [ "after 2 pm"
             , "from 2 pm"
             , "since 2pm"
             ]
  , examples (datetimeOpenInterval After (2013, 2, 17, 4, 0, 0) Hour)
             [ "after 5 days"
             ]
  , examples (datetimeOpenInterval Before (2013, 2, 12, 11, 0, 0) Hour)
             [ "before 11 am"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 12, 0, 0), (2013, 2, 12, 19, 0, 0)) Hour)
             [ "in the afternoon"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 8, 0, 0), (2013, 2, 12, 19, 0, 0)) Hour)
             [ "8am until 6"
             ]
  , examples (datetime (2013, 2, 12, 13, 30, 0) Minute)
             [ "at 1:30pm"
             , "1:30pm"
             ]
  , examples (datetime (2013, 2, 12, 4, 45, 0) Second)
             [ "in 15 minutes"
             , "in 15'"
             , "in 15"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 13, 0, 0), (2013, 2, 12, 17, 0, 0)) Hour)
             [ "after lunch"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 15, 0, 0), (2013, 2, 12, 21, 0, 0)) Hour)
             [ "after school"
             ]
  , examples (datetime (2013, 2, 12, 10, 30, 0) Minute)
             [ "10:30"
             , "approximately 1030"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 0, 0), (2013, 2, 12, 12, 0, 0)) Hour)
             [ "this morning"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "next monday"
             ]
  , examples (datetime (2013, 2, 12, 12, 0, 0) Hour)
             [ "at 12pm"
             , "at noon"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Hour)
             [ "at 12am"
             , "at midnight"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Month)
             [ "March"
             , "in March"
             ]
  , examples (datetime (2013, 2, 13, 17, 0, 0) Hour)
             [ "tomorrow afternoon at 5"
             , "at 5 tomorrow afternoon"
             , "at 5pm tomorrow"
             , "tomorrow at 5pm"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 12, 0, 0), (2013, 2, 13, 19, 0, 0)) Hour)
             [ "tomorrow afternoon"
             , "tomorrow afternoonish"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 13, 0, 0), (2013, 2, 13, 15, 0, 0)) Hour)
             [ "1pm-2pm tomorrow"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Day)
             [ "on the first"
             , "the 1st"
             ]
  , examples (datetime (2013, 2, 12, 10, 30, 0) Minute)
             [ "at 1030"
             , "around 1030"
             , "ten thirty am"
             ]
  , examples (datetime (2013, 2, 12, 19, 30, 0) Minute)
             [ "at 730 in the evening"
             , "seven thirty p.m."
             ]
  , examples (datetime (2013, 2, 13, 1, 50, 0) Minute)
             [ "tomorrow at 150ish"
             ]
  , examples (datetime (2013, 2, 12, 23, 0, 0) Hour)
             [ "tonight at 11"
             ]
  , examples (datetime (2013, 2, 12, 4, 23, 0) Minute)
    -- yes, the result is in the past, we may need to revisit
             [ "at 4:23"
             , "4:23am"
             , "four twenty-three a m"
             ]
  , examples (datetimeInterval ((2013, 3, 1, 0, 0, 0), (2013, 3, 11, 0, 0, 0)) Day)
             [ "early March"
             ]
  , examples (datetimeInterval ((2013, 3, 11, 0, 0, 0), (2013, 3, 21, 0, 0, 0)) Day)
             [ "mid March"
             ]
  , examples (datetimeInterval ((2013, 3, 21, 0, 0, 0), (2013, 4, 1, 0, 0, 0)) Day)
             [ "late March"
             ]
  , examples (datetimeInterval ((2013, 10, 25, 18, 0, 0), (2013, 10, 28, 0, 0, 0)) Hour)
             [ "last weekend of October"
             , "last week-end in October"
             , "last week end of October"
             ]
  , examples (datetimeInterval ((2013, 7, 26, 18, 0, 0), (2013, 7, 29, 0, 0, 0)) Hour)
             [ "last wkend of July"
             ]
  , examples (datetimeInterval ((2017, 10, 27, 18, 0, 0), (2017, 10, 30, 0, 0, 0)) Hour)
             [ "last weekend of October 2017"
             ]
  , examples (datetimeInterval ((2013, 8, 27, 0, 0, 0), (2013, 8, 30, 0, 0, 0)) Day)
             [ "August 27th - 29th"
             , "from August 27th - 29th"
             ]
  , examples (datetimeInterval ((2013, 10, 23, 0, 0, 0), (2013, 10, 27, 0, 0, 0)) Day)
             [ "23rd to 26th Oct"
             ]
  , examples (datetimeInterval ((2013, 9, 1, 0, 0, 0), (2013, 9, 9, 0, 0, 0)) Day)
             [ "1-8 september"
             ]
  , examples (datetimeInterval ((2013, 9, 12, 0, 0, 0), (2013, 9, 17, 0, 0, 0)) Day)
             [ "12 to 16 september"
             ]
  , examples (datetimeInterval ((2013, 8, 19, 0, 0, 0), (2013, 8, 22, 0, 0, 0)) Day)
             [ "19th To 21st aug"
             ]
  , examples (datetimeInterval ((2013, 4, 21, 0, 0, 0), (2013, 5, 1, 0, 0, 0)) Day)
             [ "end of April"
             , "at the end of April"
             ]
  , examples (datetimeInterval ((2014, 1, 1, 0, 0, 0), (2014, 1, 11, 0, 0, 0)) Day)
             [ "beginning of January"
             , "at the beginning of January"
             ]
  , examples (datetimeInterval ((2012, 9, 1, 0, 0, 0), (2012, 12, 1, 0, 0, 0)) Month)
             [ "end of 2012"
             , "at the end of 2012"
             ]
  , examples (datetimeInterval ((2017, 1, 1, 0, 0, 0), (2017, 4, 1, 0, 0, 0)) Month)
             [ "beginning of 2017"
             , "at the beginning of 2017"
             ]
  , examples (datetimeInterval ((2013, 2, 11, 0, 0, 0), (2013, 2, 14, 0, 0, 0)) Day)
             [ "beginning of this week"
             , "beginning of current week"
             , "beginning of coming week"
             , "at the beginning of this week"
             , "at the beginning of current week"
             , "at the beginning of coming week"
             ]
  , examples (datetimeInterval ((2013, 2, 4, 0, 0, 0), (2013, 2, 7, 0, 0, 0)) Day)
             [ "beginning of last week"
             , "beginning of past week"
             , "beginning of previous week"
             , "at the beginning of last week"
             , "at the beginning of past week"
             , "at the beginning of previous week"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 0, 0, 0), (2013, 2, 21, 0, 0, 0)) Day)
             [ "beginning of next week"
             , "beginning of the following week"
             , "beginning of around next week"
             , "at the beginning of next week"
             , "at the beginning of the following week"
             , "at the beginning of around next week"
             ]
  , examples (datetimeInterval ((2013, 2, 15, 0, 0, 0), (2013, 2, 18, 0, 0, 0)) Day)
             [ "end of this week"
             , "end of current week"
             , "end of coming week"
             , "at the end of this week"
             , "at the end of current week"
             , "at the end of coming week"
             ]
  , examples (datetimeInterval ((2013, 2, 8, 0, 0, 0), (2013, 2, 11, 0, 0, 0)) Day)
             [ "end of last week"
             , "end of past week"
             , "end of previous week"
             , "at the end of last week"
             , "at the end of past week"
             , "at the end of previous week"
             ]
  , examples (datetimeInterval ((2013, 2, 22, 0, 0, 0), (2013, 2, 25, 0, 0, 0)) Day)
             [ "end of next week"
             , "end of the following week"
             , "end of around next week"
             , "at the end of next week"
             , "at the end of the following week"
             , "at the end of around next week"
             ]
  ]
