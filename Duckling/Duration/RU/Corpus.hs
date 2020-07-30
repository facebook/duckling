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
  [ examples (single 1 Second)
             [ "1 сек"
             , "1 секунда"
             , "секунда"
             , "1\""
             ]
  , examples (single 15 Minute)
             [ "15 мин"
             , "пятнадцать минут"
             , "15'"
             ]
  , examples (single 30 Minute)
             [ "30 минут"
             , "пол часа"
             , "полчаса"
             , "тридцать минут"
             ]
  , examples (single 5400 Second)
             [ "полтора часа"
             , "1.5 часа"
             , "5400 секунд"
             ]
  , examples (single 8 Hour)
             [ "8 часов"
             , "8 ч"
             , "восемь часов"
             ]
  , examples (single 15 Day)
             [ "15 дней"
             , "пятнадцать дней"
             , "полмесяца"
             ]
  , examples (single 7 Week)
             [ "7 недель"
             , "семь недель"
             ]
  , examples (single 1 Month)
             [ "1 месяц"
             , "месяц"
             , "ровно месяц"
             ]
  , examples (single 6 Month)
             [ "6 месяцев"
             , "шесть месяцев"
             , "полгода"
             , "пол года"
             ]
  , examples (single 9072000 Second)
             [ "3.5 месяца"
             , "три с половиной месяца"
             , "приблизительно 3.5 месяца"
             ]
  , examples (single 3 Quarter)
             [ "3 квартала"
             ]
  , examples (single 2 Year)
             [ "2 года"
             , "два года"
             , "где-то два года"
             ]
  , examples (single 12 Hour)
             [ "12 часов"
             , "двенадцать часов"
             , "полдня"
             , "примерно полдня"
             , "пол дня"
             ]
  ]
