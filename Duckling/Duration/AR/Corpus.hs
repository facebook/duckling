-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.AR.Corpus
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
corpus = (testContext {locale = makeLocale AR Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (DurationData 1 Second)
             [ "ثانية"
             , "لحظة"
             ]
  , examples (DurationData 2 Minute)
             [ "دقيقتان"
             , "دقيقتين"
             ]
  , examples (DurationData 5 Hour)
             [ "خمسة ساعات"
             ]
  , examples (DurationData 30 Day)
             [ "30 يوم"
             ]
  , examples (DurationData 1 Week)
             [ "اسبوع"
             ]
  , examples (DurationData 7 Week)
             [ "سبع اسابيع"
             ]
  , examples (DurationData 1 Month)
             [ "شهر"
             ]
  , examples (DurationData 2 Month)
             [ "شهرين"
             ]
  , examples (DurationData 2 Year)
             [ "سنتين"
             , "سنتان"
             , "عامين"
             , "عامان"
             ]
  ]
