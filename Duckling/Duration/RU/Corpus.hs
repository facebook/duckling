-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.RU.Corpus
  ( corpus
  , negativeCorpus
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

negativeCorpus :: NegativeCorpus
negativeCorpus = (testContext {locale = makeLocale RU Nothing}, testOptions, examples)
  where
    examples =
      [ "суток"
      ]


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
             , "четверть часа"
             , "1 четверть часа"
             ]
  , examples (DurationData 30 Minute)
             [ "30 минут"
             , "пол часа"
             , "полчаса"
             , "тридцать минут"
             ]
  , examples (DurationData 45 Minute)
             [ "45 минут"
             , "3 четверти часа"
             , "три четверти часа"
             ]
  , examples (DurationData 90 Minute)
             [ "полтора часа"
             , "1.5 часа"
             ]
  , examples (DurationData 5400 Second)
             [ "5400 секунд"
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
  , examples (DurationData 105 Day)
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
  , examples (DurationData 1 Hour)
             [ "час"
             , "1 час"
             , "часик"
             , "часок"
             , "часочек"
             ]
  , examples (DurationData 5 Hour)
             [ "5 часов"
             , "5 часиков"
             , "5 часочков"
             ]
  , examples (DurationData 1 Minute)
             [ "минута"
             , "минуту"
             , "минутка"
             , "минутку"
             , "минуточка"
             , "минуточку"
             , "1 минутка"
             ]
  , examples (DurationData  4 Minute)
             [ "4 минуты"
             , "4 минутки"
             , "4 минуточки"
             ]
  , examples (DurationData 24 Hour)
             [ "сутки"
             , "1 сутки"
             ]
  , examples (DurationData 120 Hour)
             [ "5 суток"
             ]
  , examples (DurationData 115 Minute)
             [  "1 час 55 минут"
             ]
  ]
