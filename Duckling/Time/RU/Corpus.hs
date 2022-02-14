-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.RU.Corpus
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
context = testContext {locale = makeLocale RU Nothing}

corpus :: Corpus
corpus = (context, testOptions, allExamples)

negativeCorpus :: NegativeCorpus
negativeCorpus = (context, testOptions, examples)
  where
    examples =
      [ "1 отель"
      , "1 предложение"
      , "следующие 5"
      ]

allExamples :: [Example]
allExamples = concat
  [examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "сегодня"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             [ "вчера"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "позавчера"
             ]
  , examples (datetime (2013, 2, 9, 0, 0, 0) Day)
             [ "позапозавчера"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "завтра"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "послезавтра"
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "послепослезавтра"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "понедельник"
             , "в понедельник"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "понедельник, 18 февраля"
             , "18 февраля"
             , "18-е февраля"
             , "18-го февраля"
             , "восемнадцатое февраля"
             , "восемнадцатого февраля"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "вторник"
             , "во вторник"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "четверг"
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "пятница"
             , "в пятницу"
             ]
  , examples (datetime (2013, 2, 16, 0, 0, 0) Day)
             [ "суббота"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Day)
             [ "1 марта"
             , "первое марта"
             , "1-е мар."
             ]
  , examples (datetime (2015, 3, 3, 0, 0, 0) Day)
             [ "3 марта 2015"
             , "3-его марта 2015"
             , "3 мар. 2015"
             , "третьего марта 2015"
             , "третье марта 2015"
             , "к третьему марта 2015"
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "15 февраля"
             , "15.2"
             , "15 Фев"
             , "15-го Фев"
             , "15.02"
             , "пятнадцатое февраля"
             , "пятнадцатого фев"
             ]
  , examples (datetime (2013, 8, 8, 0, 0, 0) Day)
             [ "8 августа"
             , "8 Авг"
             , "восьмое августа"
             ]
  , examples (datetime (2013, 3, 0, 0, 0, 0) Month)
             [ "март"
             ]
  , examples (datetime (2014, 10, 0, 0, 0, 0) Month)
             [ "Октябрь 2014"
             ]
  , examples (datetime (2014, 11, 0, 0, 0, 0) Month)
             [ "Ноябрь 2014"
             ]
  , examples (datetime (1974, 10, 31, 0, 0, 0) Day)
             [ "31.10.1974"
             , "31.10.74"
             , "тридцать первое октября 1974"
             , "31-ое октября 1974"
             ]
  , examples (datetime (2015, 4, 14, 0, 0, 0) Day)
             [ "14 апреля 2015"
             , "четырнадцатое апреля 2015"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "Воскресенье, 10 фев."
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "Среда, 13 февраля"
             , "Среда, 13-е февраля"
             , "13-е февраля"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "Понедельник, 18 фев"
             ]
  , examples (datetime (2013, 2, 24, 0, 0, 0) Day)
             [ "двадцать четвертое фев"
             , "двадцать четвёртое февраля"
             , "двадцать четвертого февраля"
             , "24-ого февраля"
             , "24-е февраля"
             , "к двадцать четвёртому февраля"
             ]
  , examples (datetime (2015, 5, 31, 0, 0, 0) Day)
             [ "тридцать первое мая 2015"
             ]
  , examples (datetimeInterval ((2013, 2, 11, 0, 0, 0), (2013, 2, 17, 0, 0, 0)) Day)
             [ "эта неделя"
             ]
  , examples (datetime (2013, 2, 1, 0, 0, 0) Month)
             [ "этот месяц"
             ]
  , examples (datetime (2013, 1, 1, 0, 0, 0) Year)
             [ "этот год"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Week)
             [ "следующая неделя"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Week)
             [ "на следующей неделе"
             ]
  , examples (datetime (2013, 1, 0, 0, 0, 0) Month)
             [ "прошлый месяц"
             , "в прошлом месяце"
             ]
  , examples (datetime (2012, 12, 0, 0, 0, 0) Month)
             [ "позапрошлый месяц"
             , "в позапрошлом месяце"
             ]
  , examples (datetime (2013, 3, 0, 0, 0, 0) Month)
             [ "следующий месяц"
             , "в следующем месяце"
             ]
  , examples (datetime (2014, 0, 0, 0, 0, 0) Year)
             [ "следующий год"
             , "в следующем году"
             ]
  , examples (datetime (2013, 1, 1, 0, 0, 0) Quarter)
             [ "этот квартал"
             ]
  , examples (datetime (2013, 4, 1, 0, 0, 0) Quarter)
             [ "следующий квартал"
             , "в следующем квартале"
             ]
  , examples (datetime (2013, 7, 1, 0, 0, 0) Quarter)
             [ "третий квартал"
             , "в третьем квартале"
             ]
  , examples (datetime (2018, 10, 1, 0, 0, 0) Quarter)
             [ "четвертый квартал 2018"
             , "четвёртый квартал 2018"
             ]
  , examples (datetime (2013, 2, 5, 0, 0, 0) Day)
             [ "в прошлый вторник"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "следующий вторник"
             , "вторник на следующей неделе"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "следующая среда"
             , "в следующую среду"
             , "в след среду"
             ]
  , examples (datetime (2013, 2, 12, 4, 0, 0) Hour)
             [ "в 4 утра"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Hour)
             [ "в полночь"
             ]
  , examples (datetime (2013, 2, 12, 12, 0, 0) Hour)
             [ "в полдень"
             ]
  , examples (datetime (2013, 2, 12, 15, 0, 0) Hour)
             [ "в 3"
             , "3 часа"
             , "в три"
             ]
  , examples (datetime (2013, 2, 12, 3, 18, 0) Minute)
             [ "3:18 утра"
             ]
  , examples (datetime (2013, 2, 12, 15, 0, 0) Hour)
             [ "в 3 часа дня"
             , "в 15"
             , "в 15 часов"
             , "15 часов"
             , "в 15ч"
             , "в пятнадцать часов"
             , "в 3 часа пополудни"
             , "в 3 часа после полудня"
             ]
  , examples (datetime (2013, 4, 1, 18, 0, 0) Hour)
             [ "01.04. в 18 часов"
             ]
  , examples (datetime (2013, 2, 13, 17, 0, 0) Hour)
             [ "в 17 часов завтра"
             ]
  , examples (datetime (2013, 2, 12, 15, 15, 0) Minute)
             [ "15:15"
             ]
  , examples (datetime (2013, 2, 12, 20, 0, 0) Hour)
             [ "8 часов вечера"
             , "сегодня в 8 вечера"
             ]
  , examples (datetime (2013, 2, 12, 20, 0, 0) Minute)
             [ "сегодня в 20:00"
             ]
  , examples (datetime (2013, 2, 16, 9, 0, 0) Hour)
             [ "в субботу в 9 часов"
             , "в субботу в девять"
             ]
  , examples (datetime (2014, 7, 18, 19, 0, 0) Hour)
             [ "пятница, 18 июля 2014 7 часов вечера"
             ]
  , examples (datetime (2014, 7, 18, 0, 0, 0) Day)
             [ "Пт, 18 июля 2014"
             , "Пятница, 18.07.14"
             ]
  , examples (datetime (2013, 2, 12, 4, 30, 1) Second)
             [ "через 1 секунду"
             , "через секунду"
             ]
  , examples (datetime (2013, 2, 12, 4, 31, 0) Second)
             [ "через 1 минуту"
             ]
  , examples (datetime (2013, 2, 12, 4, 32, 0) Second)
             [ "через 2 минуты"
             ]
  , examples (datetime (2013, 2, 12, 5, 30, 0) Second)
             [ "через 60 минут"
             ]
  , examples (datetime (2013, 2, 12, 5, 0, 0) Second)
             [ "через 30 минут"
             ]
  , examples (datetime (2013, 2, 12, 5, 30, 0) Minute)
             [ "через 1 час"
             , "через один час"
             , "через час"
             ]
  , examples (datetime (2013, 2, 12, 6, 30, 0) Minute)
             [ "через два часа"
             ]
  , examples (datetime (2013, 2, 13, 4, 30, 0) Minute)
             [ "через 24 часа"
             ]
  , examples (datetime (2016, 2, 0, 0, 0, 0) Month)
             [ "через 3 года"
             ]
  , examples (datetime (2013, 2, 19, 4, 0, 0) Hour)
             [ "через 7 дней"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "через 1 неделю"
             , "через неделю"
             ]
  , examples (datetime (2013, 2, 5, 4, 0, 0) Hour)
             [ "7 дней назад"
             , "7 дней тому назад"
             ]
  , examples (datetime (2013, 1, 29, 4, 0, 0) Hour)
             [ "14 дней назад"
             ]
  , examples (datetime (2013, 1, 29, 0, 0, 0) Day)
             [ "две недели назад"
             ]
  , examples (datetime (2013, 2, 5, 0, 0, 0) Day)
             [ "1 неделю назад"
             , "неделю тому назад"
             ]
  , examples (datetime (2013, 1, 22, 0, 0, 0) Day)
             [ "три недели назад"
             ]
  , examples (datetime (2012, 11, 12, 0, 0, 0) Day)
             [ "три месяца назад"
             ]
  , examples (datetime (2011, 2, 0, 0, 0, 0) Month)
             [ "два года назад"
             ]
  , examples (datetimeInterval ((2013, 6, 1, 0, 0, 0), (2013, 9, 1, 0, 0, 0)) Day)
             [ "лета"
             , "лето"
             ]
  , examples (datetimeInterval ((2013, 3, 1, 0, 0, 0), (2013, 6, 1, 0, 0, 0)) Day)
             [ "весна"
             ]
  , examples (datetimeInterval ((2012, 12, 1, 0, 0, 0), (2013, 3, 1, 0, 0, 0)) Day)
             [ "зима"
             ]
  , examples (datetimeHoliday (2014, 1, 7, 0, 0, 0) Day "Рождество Христово")
             [ "рождество"
             , "Рождество Христово"
             ]
  , examples (datetimeHoliday (2014, 1, 1, 0, 0, 0) Day "Новый год")
             [ "Новый год"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 18, 0, 0), (2013, 2, 13, 0, 0, 0)) Hour)
             [ "сегодня вечером"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 18, 0, 0), (2013, 2, 14, 0, 0, 0)) Hour)
             [ "завтра вечером"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 12, 0, 0), (2013, 2, 13, 14, 0, 0)) Hour)
             [ "завтра в обед"
             ]
  , examples (datetimeInterval ((2013, 2, 11, 18, 0, 0), (2013, 2, 12, 0, 0, 0)) Hour)
             [ "вчера вечером"
             ]
  , examples (datetimeInterval ((2013, 2, 15, 18, 0, 0), (2013, 2, 18, 0, 0, 0)) Hour)
             [ "в эти выходные"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 29, 58), (2013, 2, 12, 4, 30, 0)) Second)
             [ "последние 2 секунды"
             , "последние две секунды"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 1), (2013, 2, 12, 4, 30, 4)) Second)
             [ "следующие 3 секунды"
             , "следующие три секунды"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 28, 0), (2013, 2, 12, 4, 30, 0)) Minute)
             [ "последние 2 минуты"
             , "последние две минуты"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 31, 0), (2013, 2, 12, 4, 34, 0)) Minute)
             [ "следующие 3 минуты"
             , "следующие три минуты"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 5, 0, 0), (2013, 2, 12, 8, 0, 0)) Hour)
             [ "следующие 3 часа"
             , "следующие три часа"
             ]
  , examples (datetimeInterval ((2013, 2, 10, 0, 0, 0), (2013, 2, 12, 0, 0, 0)) Day)
             [ "последние 2 дня"
             , "последние два дня"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 0, 0, 0), (2013, 2, 16, 0, 0, 0)) Day)
             [ "следующие 3 дня"
             , "следующие три дня"
             ]
  , examples (datetimeInterval ((2013, 1, 28, 0, 0, 0), (2013, 2, 11, 0, 0, 0)) Week)
             [ "последние 2 недели"
             , "последние две недели"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 0, 0, 0), (2013, 3, 11, 0, 0, 0)) Week)
             [ "следующие 3 недели"
             , "следующие три недели"
             ]
  , examples (datetimeInterval ((2012, 12, 0, 0, 0, 0), (2013, 2, 0, 0, 0, 0)) Month)
             [ "последние 2 месяца"
             , "последние два месяца"
             ]
  , examples (datetimeInterval ((2013, 3, 0, 0, 0, 0), (2013, 6, 0, 0, 0, 0)) Month)
             [ "следующие 3 месяца"
             , "следующие три месяца"
             ]
  , examples (datetimeInterval ((2011, 0, 0, 0, 0, 0), (2013, 0, 0, 0, 0, 0)) Year)
             [ "последние 2 года"
             , "последние два года"
             ]
  , examples (datetimeInterval ((2014, 0, 0, 0, 0, 0), (2017, 0, 0, 0, 0, 0)) Year)
             [ "следующие 3 года"
             , "следующие три года"
             ]
  , examples (datetimeInterval ((2013, 7, 13, 0, 0, 0), (2013, 7, 16, 0, 0, 0)) Day)
             [ "13 - 15 июля"
             , "с 13 по 15 июля"
             , "13 июля - 15 июля"
             ]
  , examples (datetimeInterval ((2013, 8, 8, 0, 0, 0), (2013, 8, 13, 0, 0, 0)) Day)
             [ "8 авг - 12 авг"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 9, 30, 0), (2013, 2, 12, 11, 1, 0)) Minute)
             [ "9:30 - 11:00"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 11, 30, 0), (2013, 2, 12, 13, 31, 0)) Minute)
             [ "11:30-13:30"
             ]
  , examples (datetime (2013, 9, 21, 1, 30, 0) Minute)
             [ "1:30 ночи сб, 21 сен."
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 0), (2013, 2, 26, 0, 0, 0)) Second)
             [ "в течение 2 недель"
             ]
  , examples (datetimeOpenInterval Before (2013, 2, 12, 14, 0, 0) Hour)
             [ "до 2 часов дня"
             ]
  , examples (datetimeOpenInterval Before (2013, 2, 13, 0, 0, 0) Hour)
             [ "до конца дня"
             ]
  , examples (datetimeOpenInterval Before (2013, 3, 1, 0, 0, 0) Month)
             [ "до конца месяца"
             ]
  , examples (datetime (2013, 2, 12, 13, 0, 0) Minute)
             [ "16:00 CET"
             ]
  , examples (datetime (2013, 2, 14, 6, 0, 0) Minute)
             [ "четверг 8:00 GMT"
             , "четверг 8:00 gmt"
             ]
  , examples (datetime (2013, 2, 12, 14, 0, 0) Hour)
             [ "сегодня в 14 часов"
             , "в 2"
             , "в 2 часа"
             ]
  , examples (datetime (2013, 2, 13, 15, 0, 0) Hour)
             [ "завтра в 15 часов"
             ]
  , examples (datetimeOpenInterval After (2013, 2, 12, 14, 0, 0) Hour)
             [ "после 14 часов"
             , "после 14ч"
             , "после 2 часов"
             ]
  , examples (datetimeOpenInterval Before (2013, 2, 12, 11, 0, 0) Hour)
             [ "до 11 часов"
             , "до 11 часов утра"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 12, 0, 0), (2013, 2, 12, 19, 0, 0)) Hour)
             [ "сегодня днем"
             , "сегодня днём"
             ]
  , examples (datetime (2013, 2, 12, 13, 30, 0) Minute)
             [ "в 13:30 дня"
             , "13:30"
             ]
  , examples (datetime (2013, 2, 12, 4, 45, 0) Second)
             [ "через 15 минут"
             ]
  , examples (datetime (2013, 2, 12, 10, 30, 0) Minute)
             [ "10:30"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "в следующий понедельник"
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
             , "с 14.10. - 15.10.2018"
             , "14.10. по 15.10.2018"
             , "с 14.10. по 15.10.2018"
             ]
  , examples (datetime (2013, 10, 10, 0, 0, 0) Day)
             [ "10.10.2013"
             ]
  , examples (datetime (2013, 2, 12, 10, 10, 0) Minute)
             [ "в 10:10"
             ]
  , examples (datetime (2013, 2, 12, 17, 10, 0) Minute)
             [ "17ч10"
             ]
  ]
