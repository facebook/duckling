-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.

{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.HE.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Resolve
import Duckling.Time.Corpus
import Duckling.Time.Types hiding (Month)
import Duckling.TimeGrain.Types hiding (add)
import Duckling.Testing.Types hiding (examples)

corpus :: Corpus
corpus = (testContext {locale = makeLocale HE Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (datetime (2013, 2, 12, 4, 30, 0) Second)
             [ "עכשיו"
             , "מייד"
             ]
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "היום"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             [ "אתמול"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "מחר"
             ]
  , examples (datetime (2013, 2, 17, 0, 0, 0) Day)
             [ "ראשון"
             , "יום ראשון"
             , "בראשון הזה"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "יום שני"
             , "שני"
             , "שני הזה"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             -- "שני השמונה עשרה לפברואר"
             [ "שני 18 לפברואר"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             -- "שלישי ה19"
             [ "שלישי"
             , "יום שלישי התשעה עשר"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "חמישי"
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "שישי"
             ]
  , examples (datetime (2013, 2, 16, 0, 0, 0) Day)
             [ "שבת"
             ]
  , examples (datetime (2013, 2, 17, 0, 0, 0) Day)
             [ "ראשון"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Day)
             -- "הראשון למרץ"
             -- "ה1 למרץ"
             [ "1 למרץ"
             ]
  , examples (datetime (2013, 3, 3, 0, 0, 0) Day)
             [ "במרץ 3"
             ]
  , examples (datetime (2013, 3, 15, 0, 0, 0) Day)
             [ "באמצע מרץ"
             ]
  , examples (datetime (2015, 3, 3, 0, 0, 0) Day)
             -- "השלישי למרץ 2015"
             [ "3 למרץ 2015"
             , "שלושה במרץ 2015"
             , "3/3/2015"
             , "3/3/15"
             , "2015-3-3"
             , "2015-03-03"
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             -- "חמש עשרה לחודש"
             -- "ב15 לחודש"
             -- "ב15 החודש"
             [
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "ה15 בפברואר"
             , "15 לפברואר"
             , "2/15"
             , "ב 2/15"
             , "פברואר 15"
             ]
  , examples (datetime (2013, 8, 8, 0, 0, 0) Day)
             [ "אוגוסט 8"
             ]
  , examples (datetime (2014, 10, 0, 0, 0, 0) Month)
             [ "אוקטובר 2014"
             ]
  , examples (datetime (1974, 10, 31, 0, 0, 0) Day)
             [ "10/31/1974"
             , "10/31/74"
             , "10-31-74"
             ]
  , examples (datetime (2015, 4, 14, 0, 0, 0) Day)
             [ "14 לאפריל 2015"
             , "אפריל 14, 2015"
             ]
  , examples (datetime (2013, 2, 22, 0, 0, 0) Day)
             [ "שישי הבא"
             ]
  , examples (datetime (2013, 3, 0, 0, 0, 0) Month)
             [ "מרץ הבא"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "ראשון, 10 לפברואר"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             -- "שני, השמונה עשרה לפברואר"
             -- "יום שני, ה18 לפברואר"
             [
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Week)
             [ "בשבוע הזה"
             ]
  , examples (datetime (2013, 2, 4, 0, 0, 0) Week)
             [ "שבוע שעבר"
             , "שבוע האחרון"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Week)
             [ "שבוע הבא"
             ]
  , examples (datetime (2013, 1, 0, 0, 0, 0) Month)
             [ "חודש שעבר"
             ]
  , examples (datetime (2013, 3, 0, 0, 0, 0) Month)
             [ "חודש הבא"
             ]
  , examples (datetime (2012, 0, 0, 0, 0, 0) Year)
             -- "שנה שעברה"
             [
             ]
  , examples (datetime (2014, 0, 0, 0, 0, 0) Year)
             [ "שנה הבאה"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "ראשון בשבוע שעבר"
             ]
  , examples (datetime (2013, 2, 5, 0, 0, 0) Day)
             [ "שלישי האחרון"
             ]
  , examples (datetime (2013, 2, 20, 0, 0, 0) Day)
             [ "רביעי שבוע הבא"
             , "רביעי הבא"
             ]
  , examples (datetime (2013, 2, 22, 0, 0, 0) Day)
             [ "שישי הבא"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "רביעי הזה"
             ]
  , examples (datetime (2013, 3, 25, 0, 0, 0) Day)
             [ "שני האחרון של מרץ"
             ]
  , examples (datetime (2014, 3, 30, 0, 0, 0) Day)
             [ "ראשון האחרון של מרץ 2014"
             ]
  , examples (datetime (2013, 10, 3, 0, 0, 0) Day)
             [ "השלישי באוקטובר"
             ]
  , examples (datetime (2013, 10, 1, 0, 0, 0) Day)
             -- "יום שלישי הראשון של אוקטובר"
             [
             ]
  , examples (datetime (2013, 2, 13, 3, 18, 0) Minute)
             [ "3:18am"
             , "3:18a"
             ]
  , examples (datetime (2013, 2, 12, 15, 0, 0) Hour)
             -- "@ 3pm"
             [ "ב 3pm"
             , "3PM"
             , "3pm"
             ]
  , examples (datetime (2013, 2, 12, 15, 0, 0) Hour)
             -- "באיזור שלוש בצהריים"
             [
             ]
  , examples (datetime (2013, 2, 12, 15, 15, 0) Minute)
             -- "3:15 בצהריים"
             -- "בשלוש ורבע בצהריים"
             [ "15:15"
             , "3:15pm"
             , "3:15PM"
             , "3:15p"
             ]
  , examples (datetime (2013, 2, 12, 15, 20, 0) Minute)
             -- "3:20 בצהריים"
             -- "3:20 צהריים"
             -- "עשרים אחרי שלוש בצהריים"
             [ "3:20p"
             ]
  , examples (datetime (2013, 2, 12, 15, 30, 0) Minute)
             -- "בשלוש וחצי בערב"
             -- "שלוש וחצי בצהריים"
             [ "15:30"
             , "3:30pm"
             , "3:30PM"
             , "330 p.m."
             , "3:30 p m"
             ]
  , examples (datetime (2013, 2, 12, 15, 23, 24) Second)
             [ "15:23:24"
             ]
  , examples (datetime (2013, 2, 12, 11, 45, 0) Minute)
             [ "רבע ל12"
             , "11:45am"
             ]
  , examples (datetime (2013, 9, 20, 19, 30, 0) Minute)
             -- "בשבע וחצי בערב ביום שישי העשרים לספטמבר"
             [
             ]
  , examples (datetime (2013, 2, 16, 9, 0, 0) Hour)
             [ "בתשע בבוקר בשבת"
             ]
  , examples (datetime (2014, 7, 18, 19, 0, 0) Minute)
             [ "שישי, יולי 18, 2014 07:00 PM"
             ]
  , examples (datetime (2013, 2, 12, 4, 32, 0) Second)
             [ "בעוד 2 דקות"
             ]
  , examples (datetime (2013, 2, 12, 5, 30, 0) Second)
             [ "בעוד 60 דקות"
             ]
  , examples (datetime (2013, 2, 12, 4, 45, 0) Second)
             [ "בעוד רבע שעה"
             ]
  , examples (datetime (2013, 2, 12, 5, 0, 0) Second)
             [ "בעוד חצי שעה"
             ]
  , examples (datetime (2013, 2, 13, 4, 30, 0) Minute)
             [ "בעוד 24 שעות"
             , "בעוד עשרים וארבע שעות"
             ]
  , examples (datetime (2013, 2, 19, 4, 0, 0) Hour)
             [ "בעוד שבעה ימים"
             ]
  , examples (datetime (2013, 2, 5, 4, 0, 0) Hour)
             [ "לפני שבעה ימים"
             ]
  , examples (datetime (2012, 11, 12, 0, 0, 0) Day)
             -- "לפני שלושה חודשים"
             [
             ]
  , examples (datetime (1954, 0, 0, 0, 0, 0) Year)
             [ "1954"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 18, 0, 0), (2013, 2, 13, 0, 0, 0)) Hour)
             [ "הערב"
             , "היום בערב"
             ]
  , examples (datetimeInterval ((2013, 2, 8, 18, 0, 0), (2013, 2, 11, 0, 0, 0)) Hour)
             [ "בסופ״ש האחרון"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 18, 0, 0), (2013, 2, 14, 0, 0, 0)) Hour)
             [ "מחר בערב"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 12, 0, 0), (2013, 2, 13, 14, 0, 0)) Hour)
             [ "מחר בצהריים"
             , "מחר צהריים"
             ]
  , examples (datetimeInterval ((2013, 2, 11, 18, 0, 0), (2013, 2, 12, 0, 0, 0)) Hour)
             [ "אתמול בערב"
             ]
  , examples (datetimeInterval ((2013, 2, 15, 18, 0, 0), (2013, 2, 18, 0, 0, 0)) Hour)
             [ "בסופ״ש הזה"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 4, 0, 0), (2013, 2, 18, 12, 0, 0)) Hour)
             [ "שני בבוקר"
             ]
  ]
