-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.ZH.Corpus
  ( corpus
  , defaultCorpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Resolve
import Duckling.Time.Corpus
import Duckling.TimeGrain.Types hiding (add)
import Duckling.Testing.Types hiding (examples)

defaultCorpus :: Corpus
defaultCorpus = corpus

corpus :: Corpus
corpus = (testContext {locale = makeLocale ZH Nothing}, testOptions, allExamples)

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
             , "宜家"
             , "而家"
             , "依家"
             ]
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "今天"
             , "今日"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             [ "昨天"
             , "昨日"
             , "尋日"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "明天"
             , "明日"
             , "聽日"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "后天"
             , "後天"
             , "後日"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "前天"
             , "前日"
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
             , "上禮拜日"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "周日, 二月十号"
             , "星期天, 二月十号"
             , "礼拜天, 二月十号"
             , "週日, 二月十號"
             , "星期天, 二月十號"
             , "禮拜天, 二月十號"
             , "禮拜日, 二月十號"
             ]
  , examples (datetime (2013, 10, 7, 0, 0, 0) Day)
             [ "十月第一个星期一"
             , "十月的第一个星期一"
             , "十月第一個星期一"
             , "十月的第一個星期一"
             ]
  , examples (datetime (2013, 2, 5, 0, 0, 0) Day)
             [ "上周二"
             , "上礼拜二"
             , "上週二"
             , "上禮拜二"
             , "上星期二"
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
             , "下星期二"
             , "下禮拜二"
             , "下礼拜二"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "这周三"
             , "这礼拜三"
             , "這週三"
             , "這禮拜三"
             , "今個星期三"
             , "今個礼拜三"
             , "今個禮拜三"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "下周三"
             , "下礼拜三"
             , "下週三"
             , "下禮拜三"
             , "下星期三"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "这周一"
             , "这礼拜一"
             , "這週一"
             , "這禮拜一"
             , "今個星期一"
             , "今個礼拜一"
             , "今個禮拜一"
             ]
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "这周二"
             , "这礼拜二"
             , "這週二"
             , "這禮拜二"
             , "今個星期二"
             , "今個礼拜二"
             , "今個禮拜二"
             , "今星期二"
             , "今礼拜二"
             , "今禮拜二"
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
             , "今個星期"
             , "今個礼拜"
             , "今個禮拜"
             ]
  , examples (datetime (2013, 2, 4, 0, 0, 0) Week)
             [ "上周"
             , "上週"
             , "上個星期"
             , "上個礼拜"
             , "上個禮拜"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Week)
             [ "下周"
             , "下週"
             , "下星期"
             , "下礼拜"
             , "下禮拜"
             ]
  , examples (datetime (2013, 1, 1, 0, 0, 0) Month)
             [ "上月"
             , "上个月"
             , "上個月"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Month)
             [ "下月"
             , "下个月"
             , "下個月"
             , "3月"
             , "3月份"
             , "三月"
             ]
  , examples (datetime (2012, 1, 1, 0, 0, 0) Year)
             [ "去年"
             , "上年"
             ]
  , examples (datetime (2013, 1, 1, 0, 0, 0) Year)
             [ "今年"
             , "这一年"
             , "這一年"
             ]
  , examples (datetime (2014, 1, 1, 0, 0, 0) Year)
             [ "明年"
             , "下年"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 29, 58), (2013, 2, 12, 4, 30, 0)) Second)
             [ "上两秒"
             , "上二秒"
             , "前两秒"
             , "前二秒"
             , "上兩秒"
             , "前兩秒"
             , "兩秒前"
             , "兩秒之前"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 1), (2013, 2, 12, 4, 30, 4)) Second)
             [ "下三秒"
             , "后三秒"
             , "後三秒"
             , "三秒後"
             , "三秒之後"
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
             , "兩分鐘前"
             , "兩分鐘之前"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 31, 0), (2013, 2, 12, 4, 34, 0)) Minute)
             [ "下三分钟"
             , "后三分钟"
             , "下三分鐘"
             , "後三分鐘"
             , "三分鐘後"
             , "三分鐘之後"
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
             , "兩小時之前"
             , "兩個鐘之前"
             , "兩小時前"
             , "兩個鐘前"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 5, 0, 0), (2013, 2, 12, 8, 0, 0)) Hour)
             [ "下三小时"
             , "后三小时"
             , "下三小時"
             , "後三小時"
             , "三小時之後"
             , "三個鐘之後"
             , "三小時後"
             , "三個鐘後"
             ]
  , examples (datetimeInterval ((2013, 2, 10, 0, 0, 0), (2013, 2, 12, 0, 0, 0)) Day)
             [ "上两天"
             , "前两天"
             , "上兩天"
             , "前兩天"
             , "兩天前"
             , "兩天之前"
             , "兩日前"
             , "兩日之前"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 0, 0, 0), (2013, 2, 16, 0, 0, 0)) Day)
             [ "下三天"
             , "后三天"
             , "後三天"
             , "三天後"
             , "三天之後"
             , "三日後"
             , "三日之後"
             ]
  , examples (datetimeInterval ((2013, 1, 28, 0, 0, 0), (2013, 2, 11, 0, 0, 0)) Week)
             [ "上两周"
             , "上二周"
             , "上兩週"
             , "上二週"
             , "兩星期前"
             , "兩星期之前"
             , "兩個禮拜前"
             , "兩個禮拜之前"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 0, 0, 0), (2013, 3, 11, 0, 0, 0)) Week)
             [ "下三周"
             , "下三个周"
             , "下三週"
             , "下三個週"
             , "三星期後"
             , "三星期之後"
             , "三個禮拜後"
             , "三個禮拜之後"
             ]
  , examples (datetimeInterval ((2012, 12, 0, 0, 0, 0), (2013, 2, 0, 0, 0, 0)) Month)
             [ "上两个月"
             , "上二个月"
             , "前两个月"
             , "前二个月"
             , "前两月"
             , "上兩個月"
             , "上二個月"
             , "前兩個月"
             , "前二個月"
             , "前兩月"
             , "兩個月前"
             , "兩個月之前"
             ]
  , examples (datetimeInterval ((2013, 3, 0, 0, 0, 0), (2013, 6, 0, 0, 0, 0)) Month)
             [ "下三个月"
             , "后三个月"
             , "後三個月"
             , "三個月後"
             , "三個月之後"
             ]
  , examples (datetimeInterval ((2011, 0, 0, 0, 0, 0), (2013, 0, 0, 0, 0, 0)) Year)
             [ "前两年"
             , "前兩年"
             , "兩年前"
             , "兩年之前"
             ]
  , examples (datetimeInterval ((2014, 0, 0, 0, 0, 0), (2017, 0, 0, 0, 0, 0)) Year)
             [ "下三年"
             , "三年後"
             , "三年之後"
             ]
  , examples (datetime (2013, 2, 12, 15, 0, 0) Hour)
             [ "三点"
             , "三點"
             , "3pm"
             , "下午三點"
             , "晏晝三點"
             , "下午三時"
             ]
  , examples (datetime (2013, 2, 12, 15, 15, 0) Minute)
             [ "下午三点十五"
             , "下午3:15"
             , "15:15"
             , "3:15pm"
             , "3:15PM"
             , "3:15p"
             , "下午三點十五"
             , "晏晝三點十五"
             ]
  , examples (datetime (2013, 2, 12, 13, 0, 0) Minute)
             [ "4pm CET"
             ]
  , examples (datetime (2015, 4, 14, 0, 0, 0) Day)
             [ "2015年4月14号"
             , "2015年4月14號"
             ]
  , examples (datetime (2013, 2, 12, 20, 0, 0) Hour)
             [ "今晚8点"
             , "今晚八点"
             , "今晚8點"
             , "今晚八點"
             ]
  , examples (datetimeHoliday (2014, 1, 1, 0, 0, 0) Day "元旦")
             [ "元旦"
             , "元旦节"
             , "元旦節"
             , "阳历新年"
             ]
  , examples (datetimeHoliday (2013, 3, 8, 0, 0, 0) Day "妇女节")
             [ "妇女节"
             , "婦女節"
             ]
  , examples (datetimeHoliday (2013, 5, 1, 0, 0, 0) Day "劳动节")
             [ "劳动节"
             , "勞動節"
             , "五一国际劳动节"
             ]
  , examples (datetimeHoliday (2013, 6, 1, 0, 0, 0) Day "儿童节")
             [ "儿童节"
             , "兒童節"
             , "国际儿童节"
             ]
  , examples (datetimeHoliday (2013, 8, 1, 0, 0, 0) Day "建军节")
             [ "建军节"
             , "八一建軍節"
             , "建軍節"
             ]
  , examples (datetimeHoliday (2013, 12, 25, 0, 0, 0) Day "圣诞节")
             [ "圣诞"
             , "聖誕"
             , "圣诞节"
             , "聖誕節"
             ]
  , examples (datetimeHoliday (2013, 4, 1, 0, 0, 0) Day "愚人节")
             [ "愚人节"
             , "愚人節"
             ]
  , examples (datetimeHoliday (2013, 10, 31, 0, 0, 0) Day "万圣节")
             [ "万圣节"
             , "萬聖節"
             ]
  , examples (datetimeHoliday (2013, 12, 20, 0, 0, 0) Day "澳门回归纪念日")
             [ "澳门回归纪念日"
             , "澳門回歸紀念日"
             ]
  , examples (datetimeHoliday (2013, 2, 14, 0, 0, 0) Day "情人节")
             [ "情人节"
             , "情人節"
             , "圣瓦伦丁节"
             ]
  , examples (datetimeHoliday (2013, 3, 15, 0, 0, 0) Day "国际消费者权益日")
             [ "国际消费者权益日"
             , "国际消費者權益日"
             , "三一五"
             ]
  , examples (datetimeHoliday (2013, 6, 1, 15, 15, 0) Minute "儿童节")
             [ "儿童节下午三点十五"
             , "兒童節下午三點十五"
             ]
  , examples (datetimeIntervalHoliday ((2013, 2, 14, 18, 0, 0), (2013, 2, 15, 0, 0, 0)) Hour "情人节")
             [ "情人节晚上"
             , "情人節晚上"
             ]
   , examples (datetimeHoliday (2013, 9, 13, 0, 0, 0) Day "赎罪日")
            [ "赎罪日"
            , "贖罪日"
            ]
 , examples (datetimeHoliday (2013, 9, 13, 0, 0, 0) Day "赎罪日")
            [ "赎罪日"
            , "贖罪日"
            ]
  , examples (datetimeHoliday (2013, 2, 13, 0, 0, 0) Day "大斋首日")
             [ "大斋首日"
             , "聖灰星期三"
             , "灰日"
             , "圣灰礼仪日"
             , "圣灰日"
             ]
  , examples (datetimeHoliday (2013, 11, 13, 0, 0, 0) Day "阿舒拉节")
             [ "阿舒拉節"
             ]
  -- wiki says 11/8
  , examples (datetimeHoliday (2013, 11, 9, 0, 0, 0) Day "克哈特普迦节")
             [ "克哈特普迦节"
             ]
   , examples (datetimeHoliday (2014, 1, 31, 0, 0, 0) Day "春节")
             [ "春节"
             , "农历新年"
             , "新春"
             , "正月正时"
             , "正月朔日"
             , "正月正時"
             ]
  , examples (datetimeHoliday (2013, 5, 30, 0, 0, 0) Day "基督圣体节")
             [ "基督聖體节"
             , "基督聖體聖血瞻礼"
             , "基督聖體聖血節"
             ]
  , examples (datetimeHoliday (2013, 11, 3, 0, 0, 0) Day "排灯节")
             [ "万灯节"
             , "印度燈節"
             , "排灯节"
             ]
  , examples (datetimeHoliday (2013, 4, 1, 0, 0, 0) Day "复活节星期一")
             [ "复活节星期一"
             , "復活節星期一"
             ]
  , examples (datetimeHoliday (2013, 3, 31, 0, 0, 0) Day "复活节")
             [ "复活节"
             , "復活節"
             , "主復活日"
             ]
  , examples (datetimeHoliday (2013, 10, 15, 0, 0, 0) Day "古尔邦节")
             [ "古尔邦节"
             , "古爾邦節"
             , "宰牲节"
             ]
  , examples (datetimeHoliday (2013, 8, 8, 0, 0, 0) Day "开斋节")
             [ "开斋节"
             , "開齋節"
             , "肉孜节"
             , "尔代节"
             , "爾代節"
             ]
  , examples (datetimeHoliday (2013, 3, 29, 0, 0, 0) Day "耶稣受难日")
             [ "主受难日"
             , "主受难节"
             , "沈默周五"
             , "聖週五"
             , "耶穌受難日"
             ]
  , examples (datetimeHoliday (2013, 3, 26, 0, 0, 0) Day "侯丽节")
             [ "侯丽节"
             , "荷麗节"
             , "洒红节"
             , "灑紅节"
             , "欢悦节"
             , "五彩節"
             , "胡里节"
             , "好利节"
             , "霍利節"
             ]
  , examples (datetimeHoliday (2013, 3, 30, 0, 0, 0) Day "圣周六")
             [ "神圣周六"
             , "聖週六"
             , "耶稣受难日翌日"
             , "主受難節翌日"
             , "復活節前夜"
             , "黑色星期六"
             ]
  , examples (datetimeHoliday (2013, 11, 4, 0, 0, 0) Day "伊斯兰新年")
             [ "伊斯兰新年"
             , "伊斯兰教新年"
             , "伊斯兰历新年"
             ]
  , examples (datetimeHoliday (2013, 6, 6, 0, 0, 0) Day "登霄节")
             [ "登霄节"
             , "夜行登霄節"
             ]
  , examples (datetimeHoliday (2014, 1, 16, 0, 0, 0) Day "印度丰收节第四天")
             [ "印度丰收节第四天"
             , "龐格爾節第四天"
             , "庞格尔节第四天"
             ]
  , examples (datetimeHoliday (2013, 4, 27, 0, 0, 0) Day "篝火节")
             [ "篝火节"
             , "犹太教篝火节"
             , "猶太教篝火節"
             ]
  , examples (datetimeHoliday (2013, 8, 4, 0, 0, 0) Day "法令之夜")
             [ "法令之夜"
             , "权力之夜"
             ]
  , examples (datetimeHoliday (2013, 4, 27, 0, 0, 0) Day "拉撒路圣周六")
             [ "拉撒路聖週六"
             , "拉匝路周六"
             ]
  , examples (datetimeHoliday (2014, 1, 15, 0, 0, 0) Day "印度丰收节第三天")
             [ "印度丰收节第三天"
             , "龐格爾節第三天"
             , "庞格尔节第三天"
             ]
  , examples (datetimeHoliday (2013, 3, 28, 0, 0, 0) Day "神圣星期四")
             [ "濯足節"
             , "神聖星期四"
             , "圣周星期四"
             , "圣周四"
             , "設立聖餐日"
             ]
  , examples (datetimeHoliday (2014, 1, 13, 0, 0, 0) Day "圣纪节")
             [ "圣纪节"
             , "聖紀節"
             ]
  , examples (datetimeHoliday (2013, 5, 6, 0, 0, 0) Day "东正教复活节星期一")
             [ "东正教复活节星期一"
             , "東正教復活節星期一"
             ]
  , examples (datetimeHoliday (2013, 5, 4, 0, 0, 0) Day "东正教圣周六")
             [ "東正教神聖週六"
             , "東正教聖週六"
             , "東正教耶稣受难日翌日"
             , "東正教主受難節翌日"
             , "東正教复活节前夜"
             ]
  , examples (datetimeHoliday (2013, 5, 3, 0, 0, 0) Day "东正教耶稣受难日")
             [ "東正教耶穌受难日"
             , "东正教主受难节"
             , "东正教圣周五"
             , "東正教聖週五"
             , "東正教沈默週五"
             ]
  , examples (datetimeHoliday (2013, 4, 28, 0, 0, 0) Day "东正教棕枝主日")
             [ "东正教棕枝主日"
             , "东正教圣枝主日"
             , "东正教聖樹主日"
             , "東正教基督苦難主日"
             ]
  , examples (datetimeHoliday (2013, 3, 24, 0, 0, 0) Day "棕枝主日")
             [ "棕枝主日"
             , "圣枝主日"
             , "聖樹主日"
             , "基督苦難主日"
             ]
  , examples (datetimeHoliday (2013, 5, 19, 0, 0, 0) Day "五旬节")
             [ "五旬节"
             , "圣灵降临节"
             , "聖靈降臨日"
             ]
  , examples (datetimeHoliday (2013, 8, 20, 0, 0, 0) Day "印度兄妹节")
             [ "印度兄妹节"
             , "拉克沙班丹節"
             ]
  , examples (datetimeHoliday (2013, 9, 25, 0, 0, 0) Day "圣会节")
              [ "圣会节"
              , "聖会節"
              ]
   , examples (datetimeHoliday (2013, 2, 12, 0, 0, 0) Day "忏悔节")
              [ "忏悔節"
              , "忏悔火曜日"
              , "煎餅星期二"
              ]
   , examples (datetimeHoliday (2013, 9, 26, 0, 0, 0) Day "西赫托拉节")
              [ "西赫托拉节"
              , "西赫妥拉节"
              , "诵经节"
              , "誦經节"
              , "转经节"
              , "轉經節"
              , "律法節"
              , "歡慶聖法節"
              ]
   , examples (datetimeHoliday (2014, 1, 14, 0, 0, 0) Day "印度丰收节")
              [ "印度丰收节"
              , "淡米爾豐收節"
              , "淡米尔丰收节"
              ]
   , examples (datetimeHoliday (2013, 8, 20, 0, 0, 0) Day "欧南节")
              [ "欧南节"
              , "欧南節"
              ]
   , examples (datetimeHoliday (2013, 7, 15, 0, 0, 0) Day "圣殿被毁日")
              [ "圣殿被毁日"
              , "禁食日"
              , "聖殿被毁日"
              ]
   , examples (datetimeHoliday (2013, 5, 26, 0, 0, 0) Day "圣三一主日")
              [ "圣三一主日"
              , "天主三一主日"
              , "天主聖三一节"
              , "聖三主日"
              , "圣三节"
              ]
   , examples (datetimeHoliday (2013, 10, 14, 0, 0, 0) Day "十胜节")
              [ "十勝節"
              , "凯旋节"
              , "圣母节"
              ]
   , examples (datetimeHoliday (2014, 1, 15, 0, 0, 0) Day "犹太植树节")
              [ "犹太植树节"
              , "猶太植樹節"
              , "图比舍巴特节"
              , "圖比舍巴特節"
              , "树木新年"
              , "樹木新年"
              ]
   , examples (datetimeHoliday (2013, 5, 24, 0, 0, 0) Day "卫塞节")
              [ "卫塞节"
              , "衛塞節"
              , "威瑟节"
              , "比萨宝蕉节"
              , "比薩寶蕉節"
              ]
   , examples (datetimeHoliday (2013, 4, 15, 0, 0, 0) Day "以色列独立日")
              [ "以色列独立日"
              , "以色列獨立日"
              , "以色列国庆节"
              , "以色列國慶節"
              ]
   , examples (datetimeHoliday (2013, 9, 13, 0, 0, 0) Day "赎罪日")
              [ "赎罪日"
              , "贖罪日"
              ]
   , examples (datetimeHoliday (2013, 5, 20, 0, 0, 0) Day "圣灵节庆日")
              [ "圣灵节庆日"
              , "聖靈節慶日"
              ]
   , examples (datetimeHoliday (2013, 10, 16, 0, 0, 0) Day "老板节")
              [ "老板节"
              , "老闆節"
              ]
   , examples (datetimeIntervalHoliday ((2013, 4, 26, 0, 0, 0), (2013, 4, 29, 0, 0, 0)) Day "全球青年服务日")
              [ "全球青年服务日"
              , "全球青年服務日"
              ]
   , examples (datetimeIntervalHoliday ((2018, 2, 19, 0, 0, 0), (2018, 3, 31, 0, 0, 0)) Day "四旬节")
              [ "2018年四旬节"
              , "2018年四旬節"
              ]
   , examples (datetimeIntervalHoliday ((2018, 12, 2, 0, 0, 0), (2018, 12, 10, 0, 0, 0)) Day "光明节")
              [ "2018年光明节"
              , "2018年修殿节"
              , "2018年献殿节"
              , "2018年獻殿节"
              , "2018年烛光节"
              , "2018年哈努卡节"
              , "2018年馬加比節"
              ]
   , examples (datetimeIntervalHoliday ((2018, 2, 14, 0, 0, 0), (2018, 4, 1, 0, 0, 0)) Day "大斋期")
              [ "2018年大斋期"
              , "2018年大齋節"
              , "2018年大斋节"
              ]
    , examples (datetimeIntervalHoliday ((2018, 10, 9, 0, 0, 0), (2018, 10, 19, 0, 0, 0)) Day "九夜节")
               [ "2018年九夜节"
               , "2018年难近母节"
               , "2018年難近母節"
               ]
   , examples (datetimeIntervalHoliday ((2018, 3, 30, 0, 0, 0), (2018, 4, 8, 0, 0, 0)) Day "逾越节")
              [ "2018年逾越节"
              , "2018年逾越節"
              ]
    , examples (datetimeIntervalHoliday ((2034, 11, 12, 0, 0, 0), (2034, 12, 12, 0, 0, 0)) Day "斋月")
               [ "2034年斋月"
               , "2034年穆斯林斋月"
               , "2034年穆斯林齋月"
               ]
   , examples (datetimeIntervalHoliday ((2018, 9, 9, 0, 0, 0), (2018, 9, 12, 0, 0, 0)) Day "犹太新年")
              [ "2018年犹太新年"
              , "2018年猶太新年"
              ]
  , examples (datetimeIntervalHoliday ((2018, 5, 19, 0, 0, 0), (2018, 5, 22, 0, 0, 0)) Day "七七节")
             [ "2018年七七节"
             , "2018年沙夫幼特节"
             , "2018年週日節"
             , "2018年收获节"
             , "2018年新果实节"
             , "2018年新果實節"
             , "2018年收穫節"
             ]
   , examples (datetimeIntervalHoliday ((2018, 9, 23, 0, 0, 0), (2018, 10, 2, 0, 0, 0)) Day "住棚节")
              [ "2018年住棚节"
              , "2018年住棚節"
              ]
  , examples (datetimeIntervalHoliday ((2016, 3, 19, 20, 30, 0), (2016, 3, 19, 21, 31, 0)) Minute "地球一小时")
             [ "2016年地球一小时"
             , "2016年地球一小時"
             ]
  ]
