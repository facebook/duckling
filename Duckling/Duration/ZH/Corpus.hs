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
  ]
