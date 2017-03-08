-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.ZH.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Lang
import Duckling.Resolve
import Duckling.Time.Corpus
import Duckling.TimeGrain.Types hiding (add)
import Duckling.Testing.Types hiding (examples)

corpus :: Corpus
corpus = (testContext {lang = ZH}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (datetime (2013, 2, 12, 4, 30, 0) Second)
             [ "现在"
             , "此时"
             , "此刻"
             , "当前"
             , "現在"
             , "此時"
             , "當前"
             ]
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "今天"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             [ "昨天"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "明天"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "后天"
             , "後天"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "前天"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "星期一"
             , "礼拜一"
             , "周一"
             , "禮拜一"
             , "週一"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "星期二"
             , "礼拜二"
             , "周二"
             , "禮拜二"
             , "週二"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "星期三"
             , "礼拜三"
             , "周三"
             , "禮拜三"
             , "週三"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "星期四"
             , "礼拜四"
             , "周四"
             , "禮拜四"
             , "週四"
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "星期五"
             , "礼拜五"
             , "周五"
             , "禮拜五"
             , "週五"
             ]
  , examples (datetime (2013, 2, 16, 0, 0, 0) Day)
             [ "星期六"
             , "礼拜六"
             , "周六"
             , "禮拜六"
             , "週六"
             ]
  , examples (datetime (2013, 2, 17, 0, 0, 0) Day)
             [ "星期日"
             , "星期天"
             , "礼拜天"
             , "周日"
             , "禮拜天"
             , "週日"
             ]
  , examples (datetimeInterval ((2013, 2, 15, 18, 0, 0), (2013, 2, 18, 0, 0, 0)) Hour)
             [ "这周末"
             , "這週末"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 4, 0, 0), (2013, 2, 18, 12, 0, 0)) Hour)
             [ "周一早上"
             , "周一早晨"
             , "礼拜一早上"
             , "礼拜一早晨"
             , "週一早上"
             , "週一早晨"
             , "禮拜一早上"
             , "禮拜一早晨"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "上周日"
             , "上星期天"
             , "上礼拜天"
             , "上週日"
             , "上星期天"
             , "上禮拜天"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "周日, 二月十号"
             , "星期天, 二月十号"
             , "礼拜天, 二月十号"
             , "週日, 二月十號"
             , "星期天, 二月十號"
             , "禮拜天, 二月十號"
             ]
  , examples (datetime (2013, 10, 7, 0, 0, 0) Day)
             [ "十月第一个星期一"
             , "十月的第一个星期一"
             ]
  , examples (datetime (2013, 2, 5, 0, 0, 0) Day)
             [ "上周二"
             , "上礼拜二"
             , "上週二"
             , "上禮拜二"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Day)
             [ "三月一号"
             , "三月一日"
             , "三月一號"
             ]
  , examples (datetime (2015, 3, 3, 0, 0, 0) Day)
             [ "2015年3月3号"
             , "2015年3月三号"
             , "2015年三月3号"
             , "2015年三月三号"
             , "2015年3月3號"
             , "2015年3月三號"
             , "2015年三月3號"
             , "2015年三月三號"
             , "3/3/2015"
             , "3/3/15"
             , "2015-3-3"
             , "2015-03-03"
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "2013年2月15号"
             , "2013年二月十五号"
             , "2月15号"
             , "二月十五号"
             , "2013年2月15號"
             , "2013年二月十五號"
             , "2月15號"
             , "二月十五號"
             , "2/15"
             ]
  , examples (datetime (1974, 10, 31, 0, 0, 0) Day)
             [ "10/31/1974"
             , "10/31/74"
             ]
  , examples (datetimeInterval ((2013, 2, 15, 4, 0, 0), (2013, 2, 15, 12, 0, 0)) Hour)
             [ "二月十五号早上"
             , "二月十五号早晨"
             , "2月15号早上"
             , "2月15号早晨"
             , "二月十五號早上"
             , "二月十五號早晨"
             , "2月15號早上"
             , "2月15號早晨"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "下周二"
             , "下週二"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "这周三"
             , "这礼拜三"
             , "這週三"
             , "這禮拜三"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "下周三"
             , "下礼拜三"
             , "下週三"
             , "下禮拜三"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "这周一"
             , "这礼拜一"
             , "這週一"
             , "這禮拜一"
             ]
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "这周二"
             , "这礼拜二"
             , "這週二"
             , "這禮拜二"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Week)
             [ "这周"
             , "这一周"
             , "这礼拜"
             , "这一礼拜"
             , "這週"
             , "這一周"
             , "這禮拜"
             , "這一禮拜"
             ]
  , examples (datetime (2013, 2, 4, 0, 0, 0) Week)
             [ "上周"
             , "上週"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Week)
             [ "下周"
             , "下週"
             ]
  , examples (datetime (2013, 1, 0, 0, 0, 0) Month)
             [ "上月"
             , "上个月"
             , "上個月"
             ]
  , examples (datetime (2013, 3, 0, 0, 0, 0) Month)
             [ "下月"
             , "下个月"
             , "下個月"
             ]
  , examples (datetime (2012, 0, 0, 0, 0, 0) Year)
             [ "去年"
             ]
  , examples (datetime (2013, 0, 0, 0, 0, 0) Year)
             [ "今年"
             , "这一年"
             , "這一年"
             ]
  , examples (datetime (2014, 0, 0, 0, 0, 0) Year)
             [ "明年"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 29, 58), (2013, 2, 12, 4, 30, 0)) Second)
             [ "上两秒"
             , "上二秒"
             , "前两秒"
             , "前二秒"
             , "上兩秒"
             , "前兩秒"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 1), (2013, 2, 12, 4, 30, 4)) Second)
             [ "下三秒"
             , "后三秒"
             , "後三秒"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 28, 0), (2013, 2, 12, 4, 30, 0)) Minute)
             [ "上两分钟"
             , "上二分钟"
             , "前两分钟"
             , "前二分钟"
             , "上兩分鐘"
             , "上二分鐘"
             , "前兩分鐘"
             , "前二分鐘"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 31, 0), (2013, 2, 12, 4, 34, 0)) Minute)
             [ "下三分钟"
             , "后三分钟"
             , "下三分鐘"
             , "後三分鐘"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 2, 0, 0), (2013, 2, 12, 4, 0, 0)) Hour)
             [ "上两小时"
             , "上二小时"
             , "前两小时"
             , "前二小时"
             , "上兩小時"
             , "上二小時"
             , "前兩小時"
             , "前二小時"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 5, 0, 0), (2013, 2, 12, 8, 0, 0)) Hour)
             [ "下三小时"
             , "后三小时"
             , "下三小時"
             , "後三小時"
             ]
  , examples (datetimeInterval ((2013, 2, 10, 0, 0, 0), (2013, 2, 12, 0, 0, 0)) Day)
             [ "上两天"
             , "前两天"
             , "上兩天"
             , "前兩天"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 0, 0, 0), (2013, 2, 16, 0, 0, 0)) Day)
             [ "下三天"
             , "后三天"
             , "後三天"
             ]
  , examples (datetimeInterval ((2013, 1, 28, 0, 0, 0), (2013, 2, 11, 0, 0, 0)) Week)
             [ "上两周"
             , "上二周"
             , "上兩週"
             , "上二週"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 0, 0, 0), (2013, 3, 11, 0, 0, 0)) Week)
             [ "下三周"
             , "下三个周"
             , "下三週"
             , "下三個週"
             ]
  , examples (datetimeInterval ((2012, 12, 0, 0, 0, 0), (2013, 2, 0, 0, 0, 0)) Month)
             [ "上两个月"
             , "上二个月"
             , "上二月"
             , "前两个月"
             , "前二个月"
             , "前两月"
             , "上兩個月"
             , "上二個月"
             , "前兩個月"
             , "前二個月"
             , "前兩月"
             ]
  , examples (datetimeInterval ((2013, 3, 0, 0, 0, 0), (2013, 6, 0, 0, 0, 0)) Month)
             [ "下三个月"
             , "后三个月"
             , "後三個月"
             ]
  , examples (datetimeInterval ((2011, 0, 0, 0, 0, 0), (2013, 0, 0, 0, 0, 0)) Year)
             [ "前两年"
             , "前兩年"
             ]
  , examples (datetimeInterval ((2014, 0, 0, 0, 0, 0), (2017, 0, 0, 0, 0, 0)) Year)
             [ "下三年"
             ]
  , examples (datetime (2013, 2, 12, 15, 0, 0) Hour)
             [ "三点"
             , "三點"
             , "3pm"
             ]
  , examples (datetime (2013, 2, 12, 15, 15, 0) Minute)
             [ "下午三点十五"
             , "下午3:15"
             , "15:15"
             , "3:15pm"
             , "3:15PM"
             , "3:15p"
             , "下午三點十五"
             ]
  , examples (datetime (2013, 2, 12, 13, 0, 0) Minute)
             [ "4pm CET"
             ]
  , examples (datetime (2015, 4, 14, 0, 0, 0) Day)
             [ "2015年4月14号"
             ]
  , examples (datetime (2013, 2, 12, 20, 0, 0) Hour)
             [ "今晚8点"
             , "今晚八点"
             , "今晚8點"
             , "今晚八點"
             ]
  , examples (datetime (2014, 1, 1, 0, 0, 0) Day)
             [ "元旦"
             , "元旦节"
             , "元旦節"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "情人节"
             , "情人節"
             ]
  , examples (datetime (2013, 3, 8, 0, 0, 0) Day)
             [ "妇女节"
             , "婦女節"
             ]
  , examples (datetime (2013, 5, 1, 0, 0, 0) Day)
             [ "劳动节"
             , "勞動節"
             ]
  , examples (datetime (2013, 6, 1, 0, 0, 0) Day)
             [ "儿童节"
             , "兒童節"
             ]
  , examples (datetime (2013, 8, 1, 0, 0, 0) Day)
             [ "建军节"
             , "建軍節"
             ]
  , examples (datetime (2013, 10, 1, 0, 0, 0) Day)
             [ "国庆"
             , "國慶"
             , "国庆节"
             , "國慶節"
             ]
  , examples (datetime (2013, 12, 25, 0, 0, 0) Day)
             [ "圣诞"
             , "聖誕"
             , "圣诞节"
             , "聖誕節"
             ]
  , examples (datetimeInterval ((2013, 10, 1, 18, 0, 0), (2013, 10, 2, 0, 0, 0)) Hour)
             [ "国庆节晚上"
             , "國慶節晚上"
             ]
  , examples (datetime (2013, 6, 1, 15, 15, 0) Minute)
             [ "儿童节下午三点十五"
             , "兒童節下午三點十五"
             ]
  ]
