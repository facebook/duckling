-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.ZH.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale ZH Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple 0)
             [ "0"
             , "〇"
             , "零"
             , "零个"
             , "0个"
             ]
  , examples (simple 1)
             [ "1"
             , "一"
             , "一个"
             , "1个"
             , "壹"
             ]
  , examples (simple 2)
             [ "2"
             , "二個"
             , "二个"
             , "貳"
             , "一對"
             , "一雙"
             ]
  , examples (simple 10)
             [ "10"
             , "十"
             , "拾"
             , "五對"
             , "五雙"
             ]
  , examples (simple 11)
             [ "11"
             , "十一"
             , "拾壹"
             ]
  , examples (simple 20)
             [ "20"
             , "二十"
             , "貳拾"
             , "廿"
             ]
  , examples (simple 60)
             [ "60"
             , "六十"
             , "陸拾"
             , "五打"
             ]
  , examples (simple 33)
             [ "33"
             , "三十三"
             , "參拾參"
             , "卅三"
             ]
  , examples (simple 96)
             [ "96"
             , "九十六"
             , "玖拾陸"
             , "八打"
             ]
  , examples (simple 203)
             [ "203"
             , "二百零三"
             , "貳佰零參"
             ]
  , examples (simple 534)
             [ "534"
             , "五百三十四"
             , "伍佰參拾肆" 
             , "五百卅四"
             ]
  , examples (simple 34567)
             [ "34567"
             , "34,567"
             , "三万四千五百六十七"
             , "三萬四千五百六十七"
             , "參萬肆仟伍佰陸拾柒"
             ]
  , examples (simple 10040)
             [ "10040"
             , "10,040"
             , "一万零四十"
             , "一萬零四十"
             , "壹萬零肆拾"
             , "一萬零卌"
             ]
  , examples (simple 1.1)
             [ "1.1"
             , "1.10"
             , "01.10"
             , "一點一"
             , "十分之十一"
             , "十份十一"
             , "一又十分一"
             ]
  , examples (simple 0.77)
             [ "0.77"
             , ".77"
             , "零點77"
             ]
  , examples (simple 34507)
             [ "34507"
             , "34,507"
             , "三万四千五百零七"
             , "三萬四千五百零七"
             , "參萬肆仟伍佰零柒"
             ]
  , examples (simple 100000)
             [ "100,000"
             , "100000"
             , "100K"
             , "100k"
             , "十万"
             , "十萬"
             , "拾萬"
             ]
  , examples (simple 3000000)
             [ "3M"
             , "3000000"
             , "3,000,000"
             , "三百万"
             , "三百萬"
             , "參佰萬"
             ]
  , examples (simple 1040000)
             [ "1,040,000"
             , "1040000"
             , "1.04M"
             , "一百零四万"
             , "一百零四萬"
             , "壹佰零肆萬"
             ]
  , examples (simple 1200000)
             [ "1,200,000"
             , "1200000"
             , "1.2M"
             , ".0012G"
             , "一百二十万"
             , "一百二十萬"
             , "壹佰貳拾萬"
             , "百二萬"
             ]
  , examples (simple (-1200000))
             [ "- 1,200,000"
             , "-1200000"
             , "负1,200,000"
             , "负 1,200,000"
             , "負 1,200,000"
             , "负1200000"
             , "负 1200000"
             , "-1.2M"
             , "-1200K"
             , "-.0012G"
             , "负一百二十万"
             , "負一百二十萬"
             , "負壹佰貳拾萬"
             ]
    , examples (simple 0.5)
             [ "0.5"
             , "一半"
             , "一半半"
             , "1半"
             , "半个"
             , "半個"
             , "零點五"
             , "二分之一"
             , "二份一"
             ]
    , examples (simple 1100)
             [ "千一"
             , "一千一百"
             ]
    , examples (simple 19000)
             [ "萬九"
             ]
    , examples (between (2,3))
             [ "兩個到三個"
             , "二至三"
             , "兩三個"
             , "三兩個"
             , "兩個或三個"
             ]
    , examples (under 20)
             [ "二十以下"
             , "最多廿個"
             ]
    , examples (above 45) 
             [ "四十五以上"
             , "至少卌五"
             , "最少四十五"
             , "起碼四十五個"
             ]
    , examples (between (3,9))
             [ "三個到九個"
             , "三至九"
             , "幾個"
             , "數個"
             , "若干個"
             ]
    , examples (between (500,600))
             [ "五六百左右"
             , "五至六百"
             , "五或六百"
             ]
    , examples (between (13,14))
             [ "十三十四個左右"
             , "十三四個"
             ]
    , examples (between (60000,70000))
             [ "六至七萬"
             , "六七萬"
             ]
    , examples (between (16000,17000))
             [ "萬六到萬七"
             , "萬六七"
             , "萬六萬七左右"
             ]
    , examples (between (13,19))
             [ "十幾個"
             , "十數個"
             ]
    , examples (between (30,90))
             [ "幾十個"
             , "數十個"
             ]
    , examples (between (3000,9000))
             [ "幾千個"
             , "數千"
             ]
    , examples (simple 90)
             [ "九十"
             ]
  ]
