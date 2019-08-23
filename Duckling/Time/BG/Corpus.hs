-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.BG.Corpus
  ( corpus
  , negativeCorpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types hiding (examples)
import Duckling.Time.Corpus
import Duckling.Time.Types hiding (Month)
import Duckling.TimeGrain.Types hiding (add)

context :: Context
context = testContext {locale = makeLocale BG Nothing}

corpus :: Corpus
corpus = (context, testOptions, allExamples)

negativeCorpus :: NegativeCorpus
negativeCorpus = (context, testOptions, examples)
  where
    examples =
      [ "един хотел"
      , "една оферта"
      , "следващи 5"
      ]

allExamples :: [Example]
allExamples = concat
  [ examples (datetime (2013, 2, 12, 4, 30, 0) Second)
             [ "сега"
             , "точно сега"
             ]
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "днес"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             [ "вчера"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "утре"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "понеделник"
             , "пон."
             , "този понеделник"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "понеделник, 18 февруари"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "вторник"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "четвъртък"
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "петък"
             ]
  , examples (datetime (2013, 2, 16, 0, 0, 0) Day)
             [ "събота"
             ]
  , examples (datetime (2013, 2, 17, 0, 0, 0) Day)
             [ "неделя"
             , "нед."
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Day)
             [ "1 март"
             , "първи март"
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "на петнадесети"
             ]
  , examples (datetime (2013, 8, 8, 0, 0, 0) Day)
             [ "8 август"
             ]
  , examples (datetime (2014, 10, 0, 0, 0, 0) Month)
             [ "октомври 2014"
             ]
  , examples (datetime (1974, 10, 31, 0, 0, 0) Day)
             [ "31.10.1974"
             , "31.10.74"
             ]
  , examples (datetime (2015, 4, 14, 0, 0, 0) Day)
             ["14 април 2015"
             ,"април 14, 2015"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "следващия вторник"
             ]
  , examples (datetime (2013, 2, 22, 0, 0, 0) Day)
             [ "по-следващия петък"
             ]
  , examples (datetime (2013, 3, 0, 0, 0, 0) Month)
             [ "следващия март"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Week)
             [ "тази седмица"
             ]
  , examples (datetime (2013, 2, 4, 0, 0, 0) Week)
             [ "последната седмица"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Week)
             [ "следващата седмица"
             ]
  , examples (datetime (2013, 1, 0, 0, 0, 0) Month)
             [ "последния месец"
             ]
  , examples (datetime (2012, 0, 0, 0, 0, 0) Year)
             [ "миналата година"
             ]
  , examples (datetime (2013, 0, 0, 0, 0, 0) Year)
             [ "тази година"
             ]
  , examples (datetime (2014, 0, 0, 0, 0, 0) Year)
             [ "следващата година"
             ]
  , examples (datetime (2013, 2, 12, 4, 0, 0) Hour)
             [ "в 4 сутринта"
             ]
  , examples (datetime (2013, 2, 12, 15, 0, 0) Hour)
             [ "в 3"
             , "3 часа"
             , "в три"
             ]
  , examples (datetime (2013, 2, 13, 3, 18, 0) Minute)
             [ "3:18 сутринта"
             ]
  , examples (datetime (2013, 2, 13, 3, 18, 0) Minute)
             [ "3:18"
             ]
  , examples (datetime (2013, 2, 12, 15, 18, 0) Minute)
             [ "3:18 след обед"
             ]
  , examples (datetime (2013, 2, 12, 20, 0, 0) Minute)
             [ "днес в 20:00"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "утре"
             ]
  , examples (datetimeOpenInterval After (2016, 2, 0, 0, 0, 0) Month)
             [ "след 3 години"
             ]
  , examples (datetimeOpenInterval After (2013, 2, 19, 4, 0, 0) Hour)
             [ "след 7 дни"
             ]
  , examples (datetimeInterval ((2013, 6, 21, 0, 0, 0), (2013, 9, 24, 0, 0, 0)) Day)
             [ "това лято"
             ]
  , examples (datetimeInterval ((2012, 12, 21, 0, 0, 0), (2013, 3, 21, 0, 0, 0)) Day)
             [ "тази зима"
             ]
  , examples (datetimeHoliday (2013, 12, 25, 0, 0, 0) Day "Christmas")
             [ "коледа"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 18, 0, 0), (2013, 2, 13, 0, 0, 0)) Hour)
             [ "тази вечер"
             ]
  ]
