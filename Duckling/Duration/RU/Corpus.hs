-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.RU.Corpus
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
corpus = (testContext {locale = makeLocale RU Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (DurationData 1 Second)
             [ "1 сек"
             , "1 секунда"
             , "секунда"
             , "1\""
             ]
  , examples (DurationData 15 Minute)
             [ "15 мин"
             , "пятнадцать минут"
             , "15'"
             ]
  , examples (DurationData 30 Minute)
             [ "30 минут"
             , "пол часа"
             , "полчаса"
             , "тридцать минут"
             ]
  , examples (DurationData 5400 Second)
             [ "полтора часа"
             , "1.5 часа"
             , "5400 секунд"
             ]
  , examples (DurationData 8 Hour)
             [ "8 часов"
             , "8 ч"
             , "восемь часов"
             ]
  , examples (DurationData 15 Day)
             [ "15 дней"
             , "пятнадцать дней"
             , "полмесяца"
             ]
  , examples (DurationData 7 Week)
             [ "7 недель"
             , "семь недель"
             ]
  , examples (DurationData 1 Month)
             [ "1 месяц"
             , "месяц"
             , "ровно месяц"
             ]
  , examples (DurationData 6 Month)
             [ "6 месяцев"
             , "шесть месяцев"
             , "полгода"
             , "пол года"
             ]
  , examples (DurationData 9072000 Second)
             [ "3.5 месяца"
             , "три с половиной месяца"
             , "приблизительно 3.5 месяца"
             ]
  , examples (DurationData 3 Quarter)
             [ "3 квартала"
             ]
  , examples (DurationData 2 Year)
             [ "2 года"
             , "два года"
             , "где-то два года"
             ]
  , examples (DurationData 12 Hour)
             [ "12 часов"
             , "двенадцать часов"
             , "полдня"
             , "примерно полдня"
             , "пол дня"
             ]
  ]
