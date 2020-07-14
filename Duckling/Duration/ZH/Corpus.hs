-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.ZH.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Duration.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types
import Duckling.TimeGrain.Types (Grain(..))

corpus :: Corpus
corpus = (testContext {locale = makeLocale ZH Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (DurationData 1 Second)
             [ "1 秒钟"
             , "一 秒鐘"
             , "一 秒"
             ]
  , examples (DurationData 1 Minute)
             [ "1 分鐘"
             , "一 分鐘"
             ]
  , examples (DurationData 1 Hour)
             [ "1 小時"
             , "一 小時"
             ]
  , examples (DurationData 5 Day)
             [ "5 天"
             , "五 天"
             , "五 日"
             ]
  , examples (DurationData 10 Month)
             [ "10 月"
             , "十 月"
             ]
  , examples (DurationData 30 Minute)
             [ "30分鐘"
             , "半個鐘"
             , "半小時"
             , "三十分鐘"
             , "卅分鐘"
             ]
  , examples (DurationData 12 Hour)
             [ "半日"
             , "半天"
             , "十二小時"
             , "十二個鐘"
             ]
  , examples (DurationData 90 Minute)
             [ "一個半小時"
             , "個半小時"
             , "個半鐘"
             , "一個半鐘"
             , "1.5小時"
             , "一個小時三十分鐘"
             , "一小時零三十分鐘"
             ]
  , examples (DurationData 130 Minute)
             [ "兩小時十分"
             , "一百三十分鐘"
             ]
  , examples (DurationData 3615 Second)
             [ "一小時零十五秒"
             , "一個鐘零十五秒"
             ]
  , examples (DurationData 45 Day)
             [ "一個半月"
             , "個半月"
             ]
  , examples (DurationData 27 Month)
             [ "兩年零三個月"
             , "廿七個月"
             ]
  , examples (DurationData 330 Second)
             [ "五個半分鐘"
             , "五點五分鐘"
             , "5.5分鐘"
             , "五分三十秒"
             , "五分半鐘"
             , "五分半"
             ]
  , examples (DurationData 90 Second)
              [ "一分半鐘"
              , "一分半"
              , "分半鐘"
              , "分半"
              ]
  , examples (DurationData 15 Minute)
              [ "3個字"
              , "三個字"
              ]
  ]
