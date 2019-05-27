-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.UK.Corpus
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
context = testContext {locale = makeLocale UK Nothing}

corpus :: Corpus
corpus = (context, testOptions, allExamples)

negativeCorpus :: NegativeCorpus
negativeCorpus = (context, testOptions, examples)
  where
    examples =
      [ "1 готель"
      , "1 пропозиція"
      , "наступний 5"
      ]

allExamples :: [Example]
allExamples = concat
  [examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "сьогодні"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             [ "вчора"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "завтра"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "понеділок"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "Понеділок, 18 лютого"
             , "18 лютого"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "вівторок"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "четвер"
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "п'ятниця"
             ]
  , examples (datetime (2013, 2, 16, 0, 0, 0) Day)
             [ "субота"
             ]
  , examples (datetime (2013, 2, 17, 0, 0, 0) Day)
             [ "неділя"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Day)
             [ "1 березня"
             , "перше березня"
             , "1 бер."
             ]
  , examples (datetime (2015, 3, 3, 0, 0, 0) Day)
             [ "3 березня 2015"
             , "3 бер. 2015"
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "15 лютого"
             , "15.2"
             , "15 Лют"
             ]
  , examples (datetime (2013, 8, 8, 0, 0, 0) Day)
             [ "8 серпня"
             , "8 Сер"
             ]
  , examples (datetime (2014, 10, 0, 0, 0, 0) Month)
             [ "Жовтень 2014"
             ]
  , examples (datetime (2014, 11, 0, 0, 0, 0) Month)
             [ "Листопад 2014"
             ]
  , examples (datetime (1974, 10, 31, 0, 0, 0) Day)
             [ "31.10.1974"
             , "31.10.74"
             ]
  , examples (datetime (2015, 4, 14, 0, 0, 0) Day)
             [ "14 квітня 2015"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "Неділя, 10 лют."
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "Середа, 13 лютого"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "Понеділок, 18 лют"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Week)
             [ "цей тиждень"
             ]
  , examples (datetime (2013, 2, 4, 0, 0, 0) Week)
             [ "минулий тиждень"
             , "минулого тижня"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Week)
             [ "наступний тиждень"
             , "на наступному тижні"
             ]
  , examples (datetime (2013, 1, 0, 0, 0, 0) Month)
             [ "минулого місяця"
             ]
  , examples (datetime (2013, 3, 0, 0, 0, 0) Month)
             [ "наступного місяця"
             ]
  , examples (datetime (2013, 1, 1, 0, 0, 0) Quarter)
             [ "цей квартал"
             ]
  , examples (datetime (2013, 4, 1, 0, 0, 0) Quarter)
             [ "наступний квартал"
             ]
  , examples (datetime (2013, 7, 1, 0, 0, 0) Quarter)
             [ "третій квартал"
             ]
  , examples (datetime (2018, 10, 1, 0, 0, 0) Quarter)
             [ "четвертий квартал 2018"
             ]
  , examples (datetime (2012, 0, 0, 0, 0, 0) Year)
             [ "в минулому році"
             ]
  , examples (datetime (2013, 0, 0, 0, 0, 0) Year)
             [ "цей рік"
             ]
  , examples (datetime (2014, 0, 0, 0, 0, 0) Year)
             [ "наступний рік"
             , "в наступному році"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "минулої неділі"
             , "в минулу неділю"
             , "неділю на минулому тижні"
             ]
  , examples (datetime (2013, 2, 5, 0, 0, 0) Day)
             [ "в минулий вівторок"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "наступного вівторка"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "наступна середа"
             ]
  , examples (datetime (2013, 2, 20, 0, 0, 0) Day)
             [ "середа наступного тижня"
             , "середу після наступної"
             ]
  , examples (datetime (2013, 2, 22, 0, 0, 0) Day)
             [ "п'ятниця після наступного"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             [ "понеділок цього тижня"
             ]
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "вівторок цього тижня"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "середа цього тижня"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "післязавтра"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "позавчора"
             ]
  , examples (datetime (2013, 3, 25, 0, 0, 0) Day)
             [ "останній понеділок у березні"
             ]
  , examples (datetime (2014, 3, 30, 0, 0, 0) Day)
             [ "останній неділю в березні 2014"
             ]
  , examples (datetime (2013, 10, 3, 0, 0, 0) Day)
             [ "третій день у жовтні"
             ]
  , examples (datetime (2014, 10, 6, 0, 0, 0) Week)
             [ "перший тиждень у жовтні 2014"
             ]
  , examples (datetime (2015, 10, 31, 0, 0, 0) Day)
             [ "останній день у жовтні 2015"
             ]
  , examples (datetime (2014, 9, 22, 0, 0, 0) Week)
             [ "останній тиждень вересня 2014"
             ]
  , examples (datetime (2013, 10, 1, 0, 0, 0) Day)
             [ "перший вівторок у жовтні"
             ]
  , examples (datetime (2014, 9, 16, 0, 0, 0) Day)
             [ "третій вівторок у вересні 2014"
             ]
  , examples (datetime (2014, 10, 1, 0, 0, 0) Day)
             [ "перша середа жовтня 2014"
             ]
  , examples (datetime (2014, 10, 8, 0, 0, 0) Day)
             [ "друга середа жовтня 2014"
             ]
  , examples (datetime (2015, 1, 13, 0, 0, 0) Day)
             [ "третій вівторок після католицького різдва 2014"
             ]
  , examples (datetime (2013, 2, 12, 4, 0, 0) Hour)
             [ "о 4 ранку"
             ]
  , examples (datetime (2013, 2, 12, 15, 0, 0) Hour)
             [ "о 3"
             , "3 години"
             , "о три"
             ]
  , examples (datetime (2013, 2, 12, 3, 18, 0) Minute)
             [ "3:18 ранку"
             ]
  , examples (datetime (2013, 2, 13, 3, 18, 0) Minute)
             [ "3:18"
             ]
  , examples (datetime (2013, 2, 12, 15, 0, 0) Hour)
             [ "о 3 годині дня"
             , "о 15"
             , "о 15 годині"
             , "15 години"
             , "о 15ч"
             ]
  , examples (datetime (2013, 4, 1, 18, 0, 0) Hour)
             [ "01.04. о 18 годині"
             ]
  , examples (datetime (2013, 2, 13, 17, 0, 0) Hour)
             [ "о 17 годині завтра"
             ]
  , examples (datetime (2013, 2, 12, 15, 15, 0) Minute)
             ["15:15"
             ]
  , examples (datetime (2013, 2, 12, 20, 0, 0) Hour)
             [ "8 години вечора"
             , "сьогодні о 8 вечора"
             ]
  , examples (datetime (2013, 2, 12, 20, 0, 0) Minute)
             [ "сьогодні о 20:00"
             ]
  , examples (datetime (2013, 9, 20, 19, 30, 0) Minute)
             [ "о 19:30 20 вер."
             ]
  , examples (datetime (2013, 2, 16, 9, 0, 0) Hour)
             [ "в суботу о 9 годині"
             ]
  , examples (datetime (2014, 7, 18, 19, 0, 0) Hour)
             [ "п'ятниця, 18 липня 2014 7 година вечора"
             ]
  , examples (datetime (2014, 7, 18, 0, 0, 0) Day)
             [ "Пт, 18 липня 2014"
             , "П'ятниця, 18.07.14"
             ]
  , examples (datetime (2013, 2, 12, 4, 30, 1) Second)
             [ "через 1 секунду"
             ]
  , examples (datetime (2013, 2, 12, 4, 31, 0) Second)
             [ "через 1 хвилину"
             ]
  , examples (datetime (2013, 2, 12, 4, 32, 0) Second)
             [ "через 2 хвилини"
             ]
  , examples (datetime (2013, 2, 12, 5, 30, 0) Second)
             [ "через 60 хвилин"
             ]
  , examples (datetime (2013, 2, 12, 5, 0, 0) Second)
             [ "через 30 хвилин"
             ]
  , examples (datetime (2013, 2, 12, 5, 30, 0) Minute)
             [ "через 1 годину"
             ]
  , examples (datetime (2013, 2, 12, 6, 30, 0) Minute)
             [ "через дві години"
             ]
  , examples (datetime (2013, 2, 13, 4, 30, 0) Minute)
             [ "через 24 години"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "завтра"
             ]
  , examples (datetime (2016, 2, 0, 0, 0, 0) Month)
             [ "через 3 роки"
             ]
  , examples (datetime (2013, 2, 19, 4, 0, 0) Hour)
             [ "через 7 днів"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "через 1 тиждень"
             ]
  , examples (datetime (2013, 2, 5, 4, 0, 0) Hour)
             [ "7 днів тому"
             ]
  , examples (datetime (2013, 1, 29, 4, 0, 0) Hour)
             [ "14 днів тому"
             ]
  , examples (datetime (2013, 1, 29, 0, 0, 0) Day)
             [ "два тижні тому"
             ]
  , examples (datetime (2013, 2, 5, 0, 0, 0) Day)
             [ "1 тиждень тому"
             ]
  , examples (datetime (2013, 1, 22, 0, 0, 0) Day)
             [ "три тижні тому"
             ]
  , examples (datetime (2012, 11, 12, 0, 0, 0) Day)
             [ "три місяці тому"
             ]
  , examples (datetime (2011, 2, 0, 0, 0, 0) Month)
             [ "два роки тому"
             ]
  , examples (datetime (2014, 1, 7, 0, 0, 0) Day)
             [ "1 рік після різдва"
             ]
  , examples (datetimeInterval ((2013, 6, 1, 0, 0, 0), (2013, 9, 1, 0, 0, 0)) Day)
             [ "це літо"
             ]
  , examples (datetimeInterval ((2013, 3, 1, 0, 0, 0), (2013, 6, 1, 0, 0, 0)) Day)
             [ "ця весна"
             ]
  , examples (datetimeInterval ((2012, 12, 1, 0, 0, 0), (2013, 3, 1, 0, 0, 0)) Day)
             [ "ця зима"
             ]
  , examples (datetimeHoliday (2014, 1, 7, 0, 0, 0) Day "Різдво Христове")
             [ "різдво"
             ]
  , examples (datetimeHoliday (2014, 1, 1, 0, 0, 0) Day "Новий рік")
             [ "Новий рік"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 18, 0, 0), (2013, 2, 13, 0, 0, 0)) Hour)
             [ "сьогодні ввечері"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 18, 0, 0), (2013, 2, 14, 0, 0, 0)) Hour)
             [ "завтра ввечері"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 12, 0, 0), (2013, 2, 13, 14, 0, 0)) Hour)
             [ "завтра в обід"
             ]
  , examples (datetimeInterval ((2013, 2, 11, 18, 0, 0), (2013, 2, 12, 0, 0, 0)) Hour)
             [ "вчора ввечері"
             ]
  , examples (datetimeInterval ((2013, 2, 15, 18, 0, 0), (2013, 2, 18, 0, 0, 0)) Hour)
             [ "в ці вихідні"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 3, 0, 0), (2013, 2, 18, 12, 0, 0)) Hour)
             [ "в понеділок вранці"
             ]
  , examples (datetimeInterval ((2013, 2, 15, 3, 0, 0), (2013, 2, 15, 12, 0, 0)) Hour)
             [ "вранці 15 лютого"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 29, 58), (2013, 2, 12, 4, 30, 0)) Second)
             [ "останні 2 секунди"
             , "останні дві секунди"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 1), (2013, 2, 12, 4, 30, 4)) Second)
             [ "наступні 3 секунди"
             , "наступні три секунди"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 28, 0), (2013, 2, 12, 4, 30, 0)) Minute)
             [ "останні 2 хвилини"
             , "останні дві хвилини"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 31, 0), (2013, 2, 12, 4, 34, 0)) Minute)
             [ "наступні 3 хвилини"
             , "наступні три хвилини"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 5, 0, 0), (2013, 2, 12, 8, 0, 0)) Hour)
             [ "наступні 3 години"
             , "наступні три години"
             ]
  , examples (datetimeInterval ((2013, 2, 10, 0, 0, 0), (2013, 2, 12, 0, 0, 0)) Day)
             [ "останні 2 дні"
             , "останні дві дні"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 0, 0, 0), (2013, 2, 16, 0, 0, 0)) Day)
             [ "наступні 3 дні"
             , "наступні три дня"
             ]
  , examples (datetimeInterval ((2013, 1, 28, 0, 0, 0), (2013, 2, 11, 0, 0, 0)) Week)
             [ "останні 2 тижні"
             , "останні два тижні"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 0, 0, 0), (2013, 3, 11, 0, 0, 0)) Week)
             [ "наступні 3 тижні"
             , "наступні три тижні"
             ]
  , examples (datetimeInterval ((2012, 12, 0, 0, 0, 0), (2013, 2, 0, 0, 0, 0)) Month)
             [ "останні 2 місяці"
             , "останні два місяці"
             ]
  , examples (datetimeInterval ((2013, 3, 0, 0, 0, 0), (2013, 6, 0, 0, 0, 0)) Month)
             [ "наступні 3 місяці"
             , "наступні три місяці"
             ]
  , examples (datetimeInterval ((2011, 0, 0, 0, 0, 0), (2013, 0, 0, 0, 0, 0)) Year)
             [ "останні 2 роки"
             , "останні два роки"
             ]
  , examples (datetimeInterval ((2014, 0, 0, 0, 0, 0), (2017, 0, 0, 0, 0, 0)) Year)
             [ "наступні 3 роки"
             , "наступні три роки"
             ]
  , examples (datetimeInterval ((2013, 7, 13, 0, 0, 0), (2013, 7, 16, 0, 0, 0)) Day)
             [ "13 - 15 липня"
             , "з 13 по 15 липня"
             , "13 липня - 15 липня"
             ]
  , examples (datetimeInterval ((2013, 8, 8, 0, 0, 0), (2013, 8, 13, 0, 0, 0)) Day)
             [ "8 сер - 12 сер"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 9, 30, 0), (2013, 2, 12, 11, 1, 0)) Minute)
             [ "9:30 - 11:00"
             ]
  , examples (datetimeInterval ((2013, 2, 14, 9, 30, 0), (2013, 2, 14, 11, 1, 0)) Minute)
             [ "в четвер з 9:30 до 11:00"
             , "Четвер 9:30 - 11:00"
             , "четвер з 9:30 до 11:00"
             ]
  , examples (datetimeInterval ((2013, 2, 14, 9, 0, 0), (2013, 2, 14, 12, 0, 0)) Hour)
             [ "Четвер вранці з 9 до 11"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 11, 30, 0), (2013, 2, 12, 13, 31, 0)) Minute)
             [ "11:30-13:30"
             ]
  , examples (datetime (2013, 9, 21, 1, 30, 0) Minute)
             [ "1:30 ночі сб, 21 вер."
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 0), (2013, 2, 26, 0, 0, 0)) Second)
             [ "протягом 2 тижнів"
             ]
  , examples (datetimeOpenInterval Before (2013, 2, 12, 14, 0, 0) Hour)
             [ "до 2 годин дня"
             ]
  , examples (datetimeOpenInterval Before (2013, 2, 13, 0, 0, 0) Hour)
             [ "до кінця дня"
             ]
  , examples (datetimeOpenInterval Before (2013, 3, 1, 0, 0, 0) Month)
             [ "до кінця місяця"
             ]
  , examples (datetime (2013, 2, 12, 13, 0, 0) Minute)
             [ "16:00 CET"
             ]
  , examples (datetime (2013, 2, 14, 6, 0, 0) Minute)
             [ "четвер 8:00 GMT"
             , "четвер 8:00 gmt"
             ]
  , examples (datetime (2013, 2, 12, 14, 0, 0) Hour)
             [ "сьогодні о 14 годині"
             , "о 2"
             ]
  , examples (datetime (2013, 2, 13, 15, 0, 0) Hour)
             [ "завтра о 15 годині"
             ]
  , examples (datetimeOpenInterval After (2013, 2, 12, 14, 0, 0) Hour)
             [ "після 14 годин"
             , "після 14ч"
             , "після 2 годин"
             ]
  , examples (datetimeOpenInterval Before (2013, 2, 12, 11, 0, 0) Hour)
             [ "до 11 години"
             , "до 11 години ранку"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 12, 0, 0), (2013, 2, 12, 19, 0, 0)) Hour)
             [ "сьогодні днем"
             ]
  , examples (datetime (2013, 2, 12, 13, 30, 0) Minute)
             [ "о 13:30 дня"
             , "13:30"
             ]
  , examples (datetime (2013, 2, 12, 4, 45, 0) Second)
             [ "через 15 хвилин"
             ]
  , examples (datetime (2013, 2, 12, 10, 30, 0) Minute)
             [ "10:30"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "наступного понеділка"
             ]
  , examples (datetime (2013, 12, 10, 0, 0, 0) Day)
             [ "10.12."
             ]
  , examples (datetimeInterval ((2013, 2, 12, 18, 30, 0), (2013, 2, 12, 19, 1, 0)) Minute)
             [ "18:30ч - 19:00ч"
             , "18:30ч/19:00ч"
             ]
  , examples (datetimeInterval ((2013, 10, 14, 0, 0, 0), (2013, 10, 16, 0, 0, 0)) Day)
             [ "14. - 15.10."
             , "14 - 15.10."
             , "14. - 15.10"
             , "14.10. - 15.10."
             , "14. - 15.10.2013"
             , "14./15.10."
             ]
  , examples (datetimeInterval ((2018, 10, 14, 0, 0, 0), (2018, 10, 16, 0, 0, 0)) Day)
             [ "14. - 15.10.18"
             , "14 - 15.10.18"
             , "14./15.10.2018"
             , "з 14.10. - 15.10.2018"
             , "14.10. по 15.10.2018"
             , "з 14.10. по 15.10.2018"
             ]
  , examples (datetime (2013, 10, 10, 0, 0, 0) Day)
             ["10.10.2013"
             ]
  , examples (datetime (2013, 2, 12, 10, 10, 0) Minute)
             [ "о 10.10"
             ]
  , examples (datetime (2013, 2, 12, 17, 10, 0) Minute)
             [ "17ч10"
             ]
  ]
