-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.AR.Corpus
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
corpus = (testContext {locale = makeLocale AR Nothing}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "اليوم"
             ]
  , examples (datetime (2013, 2, 1, 0, 0, 0) Day)
             [ "2/2013"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             [ "امس"
             , "البارحة"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "غدا"
             , "بكرة"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "الاثنين"
             , "هذا الاثنين"
             , "يوم الاثنين في 18 شباط"
             , "الاثنين 18 شباط"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "الثلاثاء"
            --  , "الثلاثاء التاسع عشر من هذا الشهر"
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "الجمعة"
             , "الجمعه"
             , "يوم الجمعة 15 شباط"
             ]
  , examples (datetime (2013, 8, 15, 0, 0, 0) Day)
             [ "الخميس 15 اغسطس"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "الخميس"
             ]
  , examples (datetime (2013, 2, 16, 0, 0, 0) Day)
             [ "السبت"
             , "السبت السادس عشر"
             ]
  , examples (datetime (2013, 2, 17, 0, 0, 0) Day)
             [ "الاحد"
             , "الأحد السابع عشرة"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Day)
             [ "الاول من اذار"
             , "الاول من مارس"
             , "1 اذار"
             ]
  , examples (datetime (2013, 4, 4, 0, 0, 0) Day)
             [ "الرابع من ابريل"
             , "الرابع من نيسان"
             , "4 ابريل"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "الثلاثاء القادم"
             , "قرابة الثلاثاء القادم"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "الاربعاء، 13 شباط"
             , "الثالث عشرة من شباط"
             ]
  -- , examples (datetime (2013, 2, 11, 0, 0, 0) Week)
  --            [ "هذا الاسبوع"
  --            , "الاسبوع الحالي"
  --            , "الاسبوع القادم"
  --            ]
  , examples (datetimeInterval ((2013, 7, 13, 0, 0, 0), (2013, 7, 16, 0, 0, 0)) Day)
             [ "يوليو 13-15"
             , "تموز 13 الى 15"
             , "13 الى 15 تموز"
             , "من 13 تموز الى 15 تموز"
             , "من 13 الى 15 يوليو"
             ]
  -- , examples (datetimeInterval ((2013, 2, 12, 18, 0, 0), (2013, 2, 13, 0, 0, 0)) Hour)
  --            [ "هذه الليلة"
  --            , "الليلة"
  --            ]

  ]
