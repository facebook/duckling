-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.JA.Corpus
  ( corpus
  , negativeCorpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Resolve
import Duckling.Time.Corpus
import Duckling.Time.Types hiding (Month, refTime)
import Duckling.TimeGrain.Types hiding (add)
import Duckling.Testing.Types hiding (examples)

corpus :: Corpus
corpus = (testContext {locale = makeLocale JA Nothing}, testOptions, allExamples)

negativeCorpus :: NegativeCorpus
negativeCorpus = (testContext {locale = makeLocale JA Nothing}, testOptions, examples)
  where
    examples =
      [ "三三時分"
      , "月"
      , "火"
      , "かよう"
      ]


allExamples :: [Example]
allExamples = concat
  [ examples (datetime (2013, 2, 12, 15, 20, 0) Minute)
             [ "15:20"
             , "15時20分"
             , "3:20 pm"
             , "3:20PM"
             , "午後3:20"
             , "午後3時20分"
             , "午後三時二十分"
             , "15時20分に"
             , "15時20分で"
             , "15時20分の"
             ]
  , examples (datetime (2013, 2, 13, 3, 20, 0) Minute)
             [ "3:20 am"
             , "3:20AM"
             , "午前3:20"
             , "午前3時20分"
             , "午前三時二十分"
             ]
  , examples (datetime (2013, 2, 12, 15, 0, 0) Hour)
             [ "15時ちょうど"
             , "15時きっかり"
             , "15時ぴったり"
             , "午後3時ちょうど"
             , "午後3時きっかり"
             , "午後3時ぴったり"
             ]
  , examples (datetime (2013, 3, 25, 0, 0, 0) Day)
             [ "3月25日"
             , "3/25"
             , "03/25に"
             , "2013年3月25日に"
             ]
  , examples (datetime (2021, 8, 6, 0, 0, 0) Day)
             [ "2021年8月6日に"
             , "令和3年8月6日に"
             ]
  , examples (datetime (2021, 3, 25, 0, 0, 0) Day)
             [ "2021年三月25日に"
             , "2021年3月25日に"
             , "2021/03/25に"
             , "2021/03/25で"
             , "2021/3/25に"
             , "2021.03.25に"
             , "2021／3／25に"
             ]
  -- on Tuesday
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "火曜日に"
             , "火曜に"
             , "(火)に"
             , "（火）に"
             , "かようびに"
             , "かように"
             , "火曜日は"
             , "火曜日で"
             ]
  -- on the 25th
  , examples (datetime (2013, 2, 25, 0, 0, 0) Day)
             [ "25日に"
             , "25日は"
             , "25日で"
             ]
  -- in June
  , examples (datetime (2013, 6, 1, 0, 0, 0) Month)
             [ "6月に"
             , "6がつに"
             , "6月中に"
             , "6月の間に"
             , "6月は"
             , "6月で"
             , "六月に"
             , "ろくがつに"
             ]
  -- in 2020
  , examples (datetime (2020, 1, 1, 0, 0, 0) Year)
             [ "2020年に"
             , "2020年は"
             , "2020年で"
             , "令和2年に"
             , "令和2年は"
             , "令和2年で"
             ]
  -- in 2018
  , examples (datetime (2018, 1, 1, 0, 0, 0) Year)
             [ "平成30年に"
             ]
  -- in 1912 = gannen Taisho
  , examples (datetime (1912, 1, 1, 0, 0, 0) Year)
             [ "大正元年に"
             ]
  -- in March 2020
  , examples (datetime (2021, 3, 1, 0, 0, 0) Month)
             [ "2021年3月に"
             , "2021年三月に"
             , "2021/03に"
             , "2021/3に"
             , "2021.03に"
             , "2021.3に"
             ]
  -- in 2 months
  , examples (datetime (2013, 4, 12, 0, 0, 0) Day)
             [ "2ヶ月で"
             ]
  -- after 25/03/2021
  , examples (datetimeOpenInterval After (2021, 3, 25, 0, 0, 0) Day)
             [ "2021/03/25以降"
             , "2021/3/25以降に"
             , "2021.03.25～"
             , "2021/03/25より後"
             , "2021.3.25から"
             ]
  -- before 25/03/2021
  , examples (datetimeOpenInterval Before (2021, 3, 25, 0, 0, 0) Day)
             [ "2021/03/25まで"
             , "2021/03/25より前"
             , "2021/03/25以前"
             ]
  -- until June 2020
  , examples (datetimeOpenInterval Before (2020, 6, 1, 0, 0, 0) Day)
             [ "2020年6月1日までに"
             ]
  -- until June 2020
  , examples (datetimeOpenInterval Before (2020, 6, 1, 0, 0, 0) Month)
             [ "2020年6月よりも前"
             , "2020年6月以前"
             ]
  -- until June
  , examples (datetimeOpenInterval Before (2013, 6, 1, 0, 0, 0) Month)
             [ "6月よりも前"
             , "6月より前"
             , "6月以前"
             ]
  -- until June 1
  , examples (datetimeOpenInterval Before (2013, 6, 1, 0, 0, 0) Day)
             [ "6月1日までに"
             ]
  -- until 2020
  , examples (datetimeOpenInterval Before (2020, 1, 1, 0, 0, 0) Year)
             [ "2020年よりも前"
             , "2020年より前"
             , "2020年以前"
             ]
  , examples (datetimeInterval ((2021, 3, 25, 0, 0, 0), (2021, 3, 31, 0, 0, 0)) Day)
             [ "2021/03/25から2021/03/30まで"
             , "2021/03/25以降2021/03/30以前"
             , "2021/03/25以降2021/03/30まで"
             , "2021/03/25より2021/03/30まで"
             , "2021/03/25~2021/03/30"
             , "2021/03/25～03/30"
             ]
  -- from 2020
  , examples (datetimeOpenInterval After (2020, 1, 1, 0, 0, 0) Year)
             [ "2020年以降"
             , "2020年以来"
             , "2020年より後"
             , "2020年よりも後"
             ]
  -- from June 2020
  , examples (datetimeOpenInterval After (2020, 6, 1, 0, 0, 0) Month)
             [ "2020年6月以降"
             , "2020年6月初め以降"
             , "2020年6月から"
             , "2020年6月～"
             ]
  -- from June to July 2020
  , examples (datetimeInterval ((2020, 6, 1, 0, 0, 0), (2020, 8, 1, 0, 0, 0)) Month)
             [ "2020年6月から7月まで"
             , "2020年6月~7月"
             , "2020年6月以降7月いっぱい"
             , "2020年6月頭から7月末まで"
             , "2020年6月初めから7月いっぱい"
             , "2020年6月以降7月にかけて"
             ]
  -- from June 2020 to July 2021
  , examples (datetimeInterval ((2020, 6, 1, 0, 0, 0), (2021, 8, 1, 0, 0, 0)) Month)
             [ "2020年6月から2021年7月まで"
             , "2020年6月～2021年7月"
             , "2020年6月から2021年7月にかけて"
             , "2020年6月以降2021年7月いっぱい"
             , "2020年6月頭から2021年7月末まで"
             , "2020年6月初めから2021年7月いっぱい"
             , "2020年6月以降2021年7月にかけて"
             ]
  -- from June
  , examples (datetimeOpenInterval After (2013, 6, 1, 0, 0, 0) Month)
             [ "6月以降"
             , "6月以来"
             , "6月より後"
             , "6月よりも後"
             , "6月から"
             ]
  -- from June to July
  , examples (datetimeInterval ((2013, 6, 1, 0, 0, 0), (2013, 8, 1, 0, 0, 0)) Month)
             [ "6月から7月まで"
             , "6月~7月"
             , "6月から7月にかけて"
             , "6月初めから7月末まで"
             , "6月頭から7月いっぱい"
             ]
  -- from 2020 to 2021
  , examples (datetimeInterval ((2020, 1, 1, 0, 0, 0), (2022, 1, 1, 0, 0, 0)) Year)
             [ "2020年から2021年まで"
             , "2020年から2021年にかけて"
             , "2020年から2021年"
             , "2020年～2021年"
             , "2020年・2021年"
             , "2020年、2021年"
             , "2020から2021年"
             , "2020～2021年"
             , "2020・2021年"
             , "2020、2021年"
             , "2020年から2021年の間"
             ]

  -- ruleDaysOfWeek
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "月に"
             , "月曜"
             , "月曜日"
             , "げつようび"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "火は"
             , "火曜"
             , "火曜日"
             , "かようび"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "水で"
             , "水曜"
             , "水曜日"
             , "すいようび"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "(木)に"
             , "木曜"
             , "木曜日"
             , "もくようび"
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "（金）に"
             , "金曜"
             , "金曜日"
             , "きんようび"
             ]
  , examples (datetime (2013, 2, 16, 0, 0, 0) Day)
             [ "土に"
             , "土曜"
             , "土曜日"
             , "どようび"
             ]
  , examples (datetime (2013, 2, 17, 0, 0, 0) Day)
             [ "日に"
             , "日曜"
             , "日曜日"
             , "にちようび"
             ]
  -- ruleMonths
  , examples (datetime (2014, 1, 1, 0, 0, 0) Month)
               [ "1月"
               , "一月"
               , "１月"
               , "いちがつ"
               ]
  , examples (datetime (2013, 2, 1, 0, 0, 0) Month)
               [ "2月"
               , "二月"
               , "２月"
               , "にがつ"
               ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Month)
               [ "3月"
               , "３月"
               , "三月"
               , "さんがつ"
               , "3月に"
               , "三月は"
               , "三月の間に"
               , "三月中に"
               ]
  , examples (datetime (2013, 4, 1, 0, 0, 0) Month)
               [ "4月"
               , "四月"
               , "４月"
               , "しがつ"
               ]
  , examples (datetime (2013, 5, 1, 0, 0, 0) Month)
               [ "5月"
               , "五月"
               , "５月"
               , "ごがつ"
               ]
  , examples (datetime (2013, 6, 1, 0, 0, 0) Month)
               [ "6月"
               , "六月"
               , "６月"
               , "ろくがつ"
               ]
  , examples (datetime (2013, 7, 1, 0, 0, 0) Month)
               [ "7月"
               , "七月"
               , "７月"
               , "しちがつ"
               ]
  , examples (datetime (2013, 8, 1, 0, 0, 0) Month)
               [ "8月"
               , "八月"
               , "８月"
               , "はちがつ"
               ]
  , examples (datetime (2013, 9, 1, 0, 0, 0) Month)
               [ "9月"
               , "九月"
               , "９月"
               , "くがつ"
               ]
  , examples (datetime (2013, 10, 1, 0, 0, 0) Month)
               [ "10月"
               , "十月"
               , "１０月"
               , "じゅうがつ"
               ]
  , examples (datetime (2013, 11, 1, 0, 0, 0) Month)
               [ "11月"
               , "十一月"
               , "１１月"
               , "じゅういちがつ"
               ]
  , examples (datetime (2013, 12, 1, 0, 0, 0) Month)
               [ "12月"
               , "十二月"
               , "１２月"
               , "じゅうにがつ"
               ]
  -- weekend
  , examples (datetimeInterval ((2013, 2, 15, 18, 0, 0), (2013, 2, 18, 0, 0, 0)) Hour)
             [ "週末"
             , "しゅうまつ"
             ]
  -- now
  , examples (datetime (2013, 2, 12, 4, 30, 0) Second)
             [ "即"
             , "いま"
             , "今すぐ"
             , "ただいま"
             , "ただちに"
             ]
  -- today
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "今日"
             , "きょう"
             , "本日"
             , "ほんじつ"
             ]
  -- tomorrow
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "明日"
             , "あした"
             , "あす"
             , "みょうにち"
             ]
  -- yesterday
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             [ "昨日"
             , "きのう"
             , "さくじつ"
             , "前日"
             ]

  -- this hour
  , examples (datetime (2013, 2, 12, 4, 0, 0) Hour)
             [ "この1時間"
             , "当1時間"
             , "現1時間"
             , "今1時間"
             ]
  -- last one hour
  , examples (datetimeInterval ((2013, 2, 12, 3, 0, 0), (2013, 2, 12, 4, 0, 0)) Hour)
             [ "過去1時間"
             ]
  -- current day
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "今日"
             , "本日"
             , "きょう"
             , "ほんじつ"
             , "今日1日"
             , "この1日"
             ]
  -- current/this week
  , examples (datetime (2013, 2, 11, 0, 0, 0) Week)
             [ "今週"
             , "当週"
             , "この週"
             , "現在の週"
             , "こんしゅう"
             , "この1週間"
             ]
  -- current/this month
  , examples (datetime (2013, 2, 1, 0, 0, 0) Month)
             [ "今月"
             , "当月"
             , "こんげつ"
             , "このひと月"
             ]
  -- "this one month"
  , examples (datetimeInterval ((2013, 1, 12, 0, 0, 0), (2013, 2, 12, 4, 30, 1)) Second)
             [ "この1ヶ月"
             ]
  -- "this two months"
  , examples (datetimeInterval ((2012, 12, 12, 0, 0, 0), (2013, 2, 12, 4, 30, 1)) Second)
               [ "この2ヶ月"
               ]
  -- current/this quarter
  , examples (datetime (2013, 1, 1, 0, 0, 0) Quarter)
             [ "今四半期"
             , "今QTR"
             , "当四半期"
             , "当QTR"
             , "現四半期"
             , "本四半期"
             , "現行四半期"
             , "現在の四半期"
             , "今の四半期"
             ]
  -- current/this year
  , examples (datetime (2013, 1, 1, 0, 0, 0) Year)
             [ "今年"
             , "本年"
             , "当年"
             ]
  -- "this two years"
  , examples (datetimeInterval ((2011, 2, 1, 0, 0, 0), (2013, 2, 12, 4, 30, 1)) Second)
             [ "この2年"
             , "この二年"
             ]

  -- last second
  , examples (datetime (2013, 2, 12, 4, 29, 59) Second)
             [ "直近1秒間"
             , "直近の1秒間"
             , "直近の1秒"
             , "直近1秒"
             , "前の1秒"
             , "前の1秒間"
             , "前秒"
             , "昨秒"
             , "最後の1秒"
             , "最後の1秒間"
             ]
  -- last minute
  , examples (datetime (2013, 2, 12, 4, 29, 0) Minute)
             [ "直近1分間"
             , "直近の1分間"
             , "直近の1分"
             , "直近1分"
             , "前の1分"
             , "前の1分間"
             , "前分"
             , "昨分"
             , "最後の1分"
             , "最後の1分間"
             ]
  -- last hour
  , examples (datetime (2013, 2, 12, 3, 0, 0) Hour)
             [ "直近1時間"
             , "直近の1時間"
             , "直近1時間"
             , "前1時間"
             , "前の1時間"
             , "最後の1時間"
             ]
  -- last week
  , examples (datetime (2013, 2, 4, 0, 0, 0) Week)
             [ "先週"
             , "せんしゅう"
             , "先の週"
             , "前週"
             , "前の週"
             , "昨週"
             ]
  -- last month
  , examples (datetime (2013, 1, 1, 0, 0, 0) Month)
             [ "先月"
             , "前月"
             , "前の月"
             , "昨月"
             ]
  -- last quarter
  , examples (datetime (2012, 10, 1, 0, 0, 0) Quarter)
             [ "前四半期"
             , "前の四半期"
             , "先四半期"
             , "先QTR"
             , "先の四半期"
             , "昨四半期"
             ]
  -- last year
  , examples (datetime (2012, 1, 1, 0, 0, 0) Year)
             [ "去年"
             , "昨年"
             , "前年"
             , "前の年"
             , "先年"
             , "先の年"
             ]
  -- last 7 days
  , examples (datetimeInterval ((2013, 2, 5, 0, 0, 0), (2013, 2, 12, 0, 0, 0)) Day)
             [ "過去7日間"
             , "直在7日間"
             ]
  -- in the last 7 days
  , examples (datetimeInterval ((2013, 2, 5, 4, 0, 0), (2013, 2, 12, 4, 30, 1)) Second)
             [ "この7日間"
             ]

  -- August 19th last year
  , examples (datetime (2012, 8, 19, 0, 0, 0) Day)
             [ "去年8月19日"
             ]

  -- in two minutes
  , examples (datetime (2013, 2, 12, 4, 32, 0) Second)
             [ "2分後"
             , "2分で"
             , "2分間で"
             , "2分経ったら"
             , "2分経ってから"
             , "2分経過後に"
             , "2分経過してから"
             , "2分経過したら"
             , "2分過ぎに"
             , "2分過ぎたら"
             ]
  -- in two months
  , examples (datetime (2013, 4, 12, 0, 0, 0) Day)
             [ "2ヶ月後"
             , "2ヶ月で"
             , "2ヶ月間で"
             , "2ヶ月経ったら"
             , "2ヶ月経ってから"
             , "2ヶ月経過後に"
             , "2ヶ月経過してから"
             , "2ヶ月経過したら"
             , "2ヶ月過ぎに"
             , "2ヶ月過ぎたら"
             ]
  -- next week
  , examples (datetime (2013, 2, 18, 0, 0, 0) Week)
             [ "来週"
             , "らいしゅう"
             ]
  -- a week after next
  , examples (datetime (2013, 2, 25, 0, 0, 0) Week)
             [ "再来週"
             , "さらいしゅう"
             ]
  -- in one week
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "一週間後"
             , "1週間後"
             ]
  ]
