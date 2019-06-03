-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.KO.Corpus
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
corpus = (testContext {locale = makeLocale KO Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (datetime (2013, 2, 12, 4, 30, 0) Second)
             [ "방금"
             , "지금"
             ]
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "오늘"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             [ "어제"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "내일"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "월요일"
             , "이번주 월요일"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "2월18일 월요일"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "화요일"
             , "19일 화요일"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "목요일"
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "금요일"
             ]
  , examples (datetime (2013, 2, 16, 0, 0, 0) Day)
             [ "토요일"
             ]
  , examples (datetime (2013, 2, 17, 0, 0, 0) Day)
             [ "일요일"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Day)
             [ "3월 1일"
             ]
  , examples (datetime (2013, 3, 3, 0, 0, 0) Day)
             [ "3월 3일"
             ]
  , examples (datetime (2015, 3, 3, 0, 0, 0) Day)
             [ "2015년 3월 3일"
             , "이천십오년 삼월 삼일"
             , "2015/3/3"
             , "2015-3-3"
             , "2015-03-03"
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "15일에"
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "2월 15일"
             , "2/15"
             ]
  , examples (datetime (2013, 8, 8, 0, 0, 0) Day)
             [ "8월 8일"
             ]
  , examples (datetime (2014, 10, 1, 0, 0, 0) Month)
             [ "2014년 10월"
             ]
  , examples (datetime (1974, 10, 31, 0, 0, 0) Day)
             [ "1974/10/31"
             , "74/10/31"
             ]
  , examples (datetime (2015, 4, 14, 0, 0, 0) Day)
             [ "2015년 4월 14일"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "다음주 화요일"
             , "다음 화요일"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Month)
             [ "다음 3월"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "2월 18일 월요일"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Week)
             [ "이번주"
             , "금주"
             ]
  , examples (datetime (2013, 2, 4, 0, 0, 0) Week)
             [ "저번주"
             , "전주"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Week)
             [ "다음주"
             , "오는주"
             ]
  , examples (datetime (2013, 1, 1, 0, 0, 0) Month)
             [ "저번달"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Month)
             [ "다음달"
             ]
  , examples (datetime (2013, 1, 1, 0, 0, 0) Quarter)
             [ "이번분기"
             ]
  , examples (datetime (2013, 4, 1, 0, 0, 0) Quarter)
             [ "다음분기"
             ]
  , examples (datetime (2013, 7, 1, 0, 0, 0) Quarter)
             [ "삼분기"
             ]
  , examples (datetime (2018, 10, 1, 0, 0, 0) Quarter)
             [ "2018년 4분기"
             ]
  , examples (datetime (2012, 1, 1, 0, 0, 0) Year)
             [ "작년"
             , "12년"
             ]
  , examples (datetime (2013, 1, 1, 0, 0, 0) Year)
             [ "올해"
             ]
  , examples (datetime (2014, 1, 1, 0, 0, 0) Year)
             [ "내년"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "저번주 일요일"
             , "지난주 일요일"
             , "지난 일요일"
             , "저번 일요일"
             ]
  , examples (datetime (2013, 2, 5, 0, 0, 0) Day)
             [ "저번주 화요일"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "다음주 화요일"
             ]
  , examples (datetime (2013, 2, 20, 0, 0, 0) Day)
             [ "다음주 수요일"
             ]
  , examples (datetime (2013, 2, 22, 0, 0, 0) Day)
             [ "다음주 금요일"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "이번주 월요일"
             ]
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "이번주 화요일"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "이번주 수요일"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "내일모레"
             , "모레"
             ]
  , examples (datetime (2013, 2, 13, 17, 0, 0) Hour)
             [ "내일 저녁다섯시"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "엊그제"
             , "그제"
             ]
  , examples (datetime (2013, 2, 10, 8, 0, 0) Hour)
             [ "엊그제 아침8시"
             , "엊그제 오전8시"
             ]
  , examples (datetime (2013, 3, 25, 0, 0, 0) Day)
             [ "3월 마지막 월요일"
             ]
  , examples (datetime (2014, 3, 30, 0, 0, 0) Day)
             [ "2014년 3월 마지막일요일"
             ]
  , examples (datetime (2013, 10, 3, 0, 0, 0) Day)
             [ "10월 3일"
             ]
  , examples (datetime (2014, 10, 6, 0, 0, 0) Week)
             [ "2014년 10월 첫번째주"
             ]
  , examples (datetime (2015, 10, 31, 0, 0, 0) Day)
             [ "2015년 10월 마지막날"
             ]
  , examples (datetime (2014, 9, 22, 0, 0, 0) Week)
             [ "2014년 9월 마지막주"
             ]
  , examples (datetime (2013, 10, 1, 0, 0, 0) Day)
             [ "10월 첫째화요일"
             ]
  , examples (datetime (2014, 9, 16, 0, 0, 0) Day)
             [ "2014년 9월 셋째화요일"
             , "2014년 9월 세번째화요일"
             ]
  , examples (datetime (2014, 10, 1, 0, 0, 0) Day)
             [ "2014년 10월 첫번째 수요일"
             , "2014년 10월 첫째 수요일"
             ]
  , examples (datetime (2014, 10, 8, 0, 0, 0) Day)
             [ "2014년 10월 두번째 수요일"
             , "2014년 10월 둘째 수요일"
             ]
  , examples (datetime (2013, 2, 13, 3, 0, 0) Hour)
             [ "아침 3시"
             , "오전 세시"
             , "3AM"
             ]
  , examples (datetime (2013, 2, 13, 3, 18, 0) Minute)
             [ "3:18am"
             , "3:18a"
             ]
  , examples (datetime (2013, 2, 12, 15, 0, 0) Hour)
             [ "오후 세시"
             , "3PM"
             ]
  , examples (datetime (2013, 2, 12, 15, 20, 0) Minute)
             [ "오후세시이십분"
             , "3:20p"
             , "15:20"
             , "3:20pm"
             , "3:20PM"
             ]
  , examples (datetime (2013, 2, 12, 15, 30, 0) Minute)
             [ "오후세시반"
             , "15:30"
             , "3:30pm"
             , "3:30PM"
             ]
  , examples (datetime (2013, 2, 12, 15, 45, 0) Minute)
             [ "네시십오분전"
             ]
  , examples (datetime (2013, 2, 12, 15, 30, 0) Minute)
             [ "3:30"
             , "세시반"
             ]
  , examples (datetime (2013, 2, 12, 15, 23, 24) Second)
             [ "15:23:24"
             , "세시이십삼분이십사초"
             ]
  , examples (datetime (2013, 2, 12, 20, 0, 0) Hour)
             [ "오늘밤 8시"
             , "저녁 8시"
             ]
  , examples (datetime (2013, 9, 20, 19, 30, 0) Minute)
             [ "9월 20일 저녁 7시 30분"
             ]
  , examples (datetime (2013, 2, 16, 9, 0, 0) Hour)
             [ "토요일 9시"
             ]
  , examples (datetime (2014, 7, 18, 19, 0, 0) Hour)
             [ "2014년 7월 18일 금요일 오후 7시"
             ]
  , examples (datetime (2013, 2, 12, 4, 30, 1) Second)
             [ "1초안에"
             ]
  , examples (datetime (2013, 2, 12, 4, 31, 0) Second)
             [ "일분안에"
             , "일분내에"
             ]
  , examples (datetime (2013, 2, 12, 4, 32, 0) Second)
             [ "이분안에"
             , "이분내에"
             ]
  , examples (datetime (2013, 2, 12, 5, 30, 0) Minute)
             [ "한시간안에"
             , "한시간내"
             ]
  , examples (datetime (2013, 2, 12, 6, 0, 0) Second)
             [ "한시간반안"
             , "한시간반내"
             ]
  , examples (datetime (2013, 2, 12, 7, 0, 0) Second)
             [ "두시간반안"
             , "두시간반내"
             ]
  , examples (datetime (2013, 2, 12, 7, 30, 0) Minute)
             [ "몇시간안"
             , "몇시간내"
             ]
  , examples (datetimeOpenInterval After (2013, 2, 12, 7, 30, 0) Minute)
             [ "몇시간후"
             , "몇시간이후"
             ]
  , examples (datetime (2013, 2, 13, 4, 30, 0) Minute)
             [ "24시간안에"
             , "24시간내"
             ]
  , examples (datetime (2013, 2, 13, 4, 0, 0) Hour)
             [ "하루안에"
             , "하루내"
             ]
  , examples (datetime (2016, 2, 1, 0, 0, 0) Month)
             [ "삼년안에"
             , "삼년내"
             ]
  , examples (datetime (2013, 2, 19, 4, 0, 0) Hour)
             [ "7일안에"
             , "7일내"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "1주일안에"
             , "1주일내"
             ]
  , examples (datetime (2013, 2, 12, 6, 0, 0) Second)
             [ "약 한시간반 안에"
             ]
  , examples (datetime (2013, 2, 5, 4, 0, 0) Hour)
             [ "7일전"
             ]
  , examples (datetime (2013, 1, 29, 4, 0, 0) Hour)
             [ "14일전"
             , "14일전에"
             ]
  , examples (datetime (2013, 1, 22, 0, 0, 0) Day)
             [ "3주전"
             , "3주이전"
             ]
  , examples (datetime (2011, 2, 1, 0, 0, 0) Month)
             [ "2년전"
             , "2년이전"
             ]
  , examples (datetime (1954, 1, 1, 0, 0, 0) Year)
             [ "1954"
             ]
  , examples (datetimeInterval ((2013, 6, 21, 0, 0, 0), (2013, 9, 24, 0, 0, 0)) Day)
             [ "이번여름"
             ]
  , examples (datetimeInterval ((2012, 12, 21, 0, 0, 0), (2013, 3, 21, 0, 0, 0)) Day)
             [ "이번겨울"
             ]
  , examples (datetime (2013, 12, 25, 0, 0, 0) Day)
             [ "크리스마스"
             ]
  , examples (datetime (2013, 12, 24, 0, 0, 0) Day)
             [ "크리스마스이브"
             ]
  , examples (datetime (2014, 1, 1, 0, 0, 0) Day)
             [ "신정"
             , "설날"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Day)
             [ "삼일절"
             ]
  , examples (datetime (2013, 5, 5, 0, 0, 0) Day)
             [ "어린이날"
             ]
  , examples (datetime (2013, 6, 6, 0, 0, 0) Day)
             [ "현충일"
             ]
  , examples (datetime (2013, 6, 17, 0, 0, 0) Day)
             [ "제헌절"
             ]
  , examples (datetime (2013, 8, 15, 0, 0, 0) Day)
             [ "광복절"
             ]
  , examples (datetime (2013, 10, 3, 0, 0, 0) Day)
             [ "개천절"
             ]
  , examples (datetime (2013, 10, 9, 0, 0, 0) Day)
             [ "한글날"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 18, 0, 0), (2013, 2, 13, 0, 0, 0)) Hour)
             [ "오늘저녁"
             , "오늘밤"
             ]
  , examples (datetimeInterval ((2013, 2, 8, 18, 0, 0), (2013, 2, 11, 0, 0, 0)) Hour)
             [ "저번주말"
             , "지난주말"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 18, 0, 0), (2013, 2, 14, 0, 0, 0)) Hour)
             [ "내일저녁"
             , "내일밤"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 12, 0, 0), (2013, 2, 13, 14, 0, 0)) Hour)
             [ "내일점심"
             ]
  , examples (datetimeInterval ((2013, 2, 11, 18, 0, 0), (2013, 2, 12, 0, 0, 0)) Hour)
             [ "어제저녁"
             ]
  , examples (datetimeInterval ((2013, 2, 15, 18, 0, 0), (2013, 2, 18, 0, 0, 0)) Hour)
             [ "이번주말"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 4, 0, 0), (2013, 2, 18, 12, 0, 0)) Hour)
             [ "월요일 아침"
             ]
  , examples (datetimeInterval ((2013, 2, 15, 4, 0, 0), (2013, 2, 15, 12, 0, 0)) Hour)
             [ "2월 15일 아침"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 29, 58), (2013, 2, 12, 4, 30, 0)) Second)
             [ "지난 2초"
             , "지난 이초"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 1), (2013, 2, 12, 4, 30, 4)) Second)
             [ --"다음 3초" t14899520
               --"다음 삼초" t14899520
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 28, 0), (2013, 2, 12, 4, 30, 0)) Minute)
             [ "지난 2분"
             , "지난 이분"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 31, 0), (2013, 2, 12, 4, 34, 0)) Minute)
             [ "다음 3분"
             , "다음 삼분"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 3, 0, 0), (2013, 2, 12, 4, 0, 0)) Hour)
             [ "지난 1시간"
             , "지난 한시간"
             ]
  , examples (datetimeInterval ((2013, 2, 11, 4, 0, 0), (2013, 2, 12, 4, 0, 0)) Hour)
             [ "지난 24시간"
             , "지난 스물네시간"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 5, 0, 0), (2013, 2, 12, 8, 0, 0)) Hour)
             [ "다음 3시간"
             ]
  , examples (datetimeInterval ((2013, 2, 10, 0, 0, 0), (2013, 2, 12, 0, 0, 0)) Day)
             [ --"지난 2일" t14899546
             ]
  , examples (datetimeInterval ((2013, 2, 13, 0, 0, 0), (2013, 2, 16, 0, 0, 0)) Day)
             [ --"다음 3일" t14899546
             --, "다음 몇일" t14899546
             ]
  , examples (datetimeInterval ((2013, 1, 28, 0, 0, 0), (2013, 2, 11, 0, 0, 0)) Week)
             [ "지난 2주"
             , "지난 이주"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 0, 0, 0), (2013, 3, 11, 0, 0, 0)) Week)
             [ "다음 3주"
             , "다음 삼주"
             ]
  , examples (datetimeInterval ((2012, 12, 1, 0, 0, 0), (2013, 2, 1, 0, 0, 0)) Month)
             [ "지난 2달"
             , "지난 두달"
             ]
  , examples (datetimeInterval ((2013, 3, 1, 0, 0, 0), (2013, 6, 1, 0, 0, 0)) Month)
             [ "다음 3달"
             , "다음 세달"
             ]
  , examples (datetimeInterval ((2011, 1, 1, 0, 0, 0), (2013, 1, 1, 0, 0, 0)) Year)
             [ --"지난 2년" t14899546
             --, "지난 이년" t14899546
             ]
  , examples (datetimeInterval ((2014, 1, 1, 0, 0, 0), (2017, 1, 1, 0, 0, 0)) Year)
             [ "다음 3년"
             , "다음 삼년"
             ]
  , examples (datetimeInterval ((2013, 7, 13, 0, 0, 0), (2013, 7, 16, 0, 0, 0)) Day)
             [ "7월 13일 - 15일"
             , "7월 13일 부터 15일"
             ]
  , examples (datetimeInterval ((2013, 8, 8, 0, 0, 0), (2013, 8, 14, 0, 0, 0)) Day)
             [ "8월 8일 - 8월 13일"
             , "8월 8일부터 8월 13일"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 9, 30, 0), (2013, 2, 12, 11, 1, 0)) Minute)
             [ "9:30 부터 11:00"
             , "9:30 ~ 11:00"
             ]
  , examples (datetimeInterval ((2013, 2, 14, 9, 30, 0), (2013, 2, 14, 11, 1, 0)) Minute)
             [ "목요일 9:30 부터 11:00"
             , "목요일 9:30 ~ 11:00"
             ]
  , examples (datetimeInterval ((2013, 2, 14, 9, 0, 0), (2013, 2, 14, 12, 0, 0)) Hour)
             [ "목요일 오전9시 부터 오전11시"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 11, 30, 0), (2013, 2, 12, 13, 31, 0)) Minute)
             [ "11:30-1:30"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 0), (2013, 2, 26, 0, 0, 0)) Second)
             [ "2주 이내에"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 0), (2013, 2, 12, 14, 0, 0)) Second)
             [ "오후 2시까지"
             ]
  , examples (datetime (2013, 2, 12, 13, 0, 0) Minute)
             [ "4pm CET"
             ]
  , examples (datetime (2013, 2, 14, 6, 0, 0) Minute)
             [ "목요일 8:00 GMT"
             , "목요일 8:00 gmt"
             ]
  , examples (datetime (2013, 2, 12, 14, 0, 0) Hour)
             [ "오늘 오후두시에"
             , "오후두시에"
             ]
  , examples (datetime (2013, 4, 25, 16, 0, 0) Hour)
             [ "4/25 오후4시에"
             ]
  , examples (datetime (2013, 2, 13, 15, 0, 0) Hour)
             [ "내일 3pm"
             ]
  , examples (datetimeOpenInterval After (2013, 2, 12, 14, 0, 0) Hour)
             [ "오후2시 이후"
             ]
  , examples (datetimeOpenInterval After (2013, 2, 17, 4, 0, 0) Hour)
             [ "5일 후"
             ]
  , examples (datetimeOpenInterval Before (2013, 2, 12, 11, 0, 0) Hour)
             [ "오전11시 전"
             , "오전11시 이전"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 12, 0, 0), (2013, 2, 12, 19, 0, 0)) Hour)
             [ "오후에"
             ]
  , examples (datetime (2013, 2, 12, 4, 45, 0) Second)
             [ "15분안"
             ]
  , examples (datetimeOpenInterval After (2013, 2, 12, 12, 0, 0) Hour)
             [ "점심이후"
             ]
  , examples (datetime (2013, 2, 12, 10, 30, 0) Minute)
             [ "10:30"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 0, 0), (2013, 2, 12, 12, 0, 0)) Hour)
             [ "이번아침"
             ]
  , examples (datetime (2013, 2, 12, 12, 0, 0) Hour)
             [ "오후12시"
             , "정오"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Hour)
             [ "오전12시"
             , "자정"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Month)
             [ "3월"
             , "3월에"
             ]
  ]
