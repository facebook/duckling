-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.MN.Corpus
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
corpus = (testContext {locale = makeLocale MN Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (DurationData 1 Second)
             [ "1 сек"
             , "1 секунд"
             , "секунд"
             , "1\""
             ]
  , examples (DurationData 15 Minute)
             [ "15 мин"
             , "арван таван минут"
             , "15'"
             ]
  , examples (DurationData 30 Minute)
             [ "30 минут"
             , "хагас цаг"
             , "гучин минут"
             ]
  , examples (DurationData 5400 Second)
             [ "цаг хагас"
             , "1.5 цаг"
             , "5400 секунд"
             ]
  , examples (DurationData 8 Hour)
             [ "8 цаг"
             , "8 ц"
             , "найман цаг"
             ]
  , examples (DurationData 15 Day)
             [ "15 өдөр"
             , "арван таван өдөр"
             , "хагас сар"
             ]
  , examples (DurationData 7 Week)
             [ "7 долоо хоног"
             , "долоон долоо хоног"
             ]
  , examples (DurationData 1 Month)
             [ "1 сар"
             , "сар"
             , "яг сар"
             ]
  , examples (DurationData 6 Month)
             [ "6 сар"
             , "зургаан сар"
             , "хагас жил"
             ]
  , examples (DurationData 9072000 Second)
             [ "3.5 сар"
             , "гурван сар хагас"
             , "ойролцоогоор 3.5 сар"
             ]
  , examples (DurationData 3 Quarter)
             [ "3 квартал"
             ]
  , examples (DurationData 2 Year)
             [ "2 жил"
             , "хоёр жил"
             , "ойролцоогоор хоёр жил"
             ]
  , examples (DurationData 12 Hour)
             [ "12 цаг"
             , "арван хоёр цаг"
             , "үд дунд"
             , "ойролцоогоор үд дунд"
             ]
  ]
