-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.BG.Corpus
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
corpus = (testContext {locale = makeLocale BG Nothing}, testOptions, allExamples)

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
             , "петнадесет минути"
             , "15'"
             ]
  , examples (DurationData 30 Minute)
             [ "30 минути"
             , "половин час"
             , "тридесет минути"
             ]
  , examples (DurationData 90 Minute)
             [ "час и половина"
             , "90 мин"
             ]
  , examples (DurationData 5400 Second)
             [ "1.5 часа"
             , "5400 секунди"
             ]
  , examples (DurationData 8 Hour)
             [ "8 часа"
             , "8 ч"
             , "осем часа"
             ]
  , examples (DurationData 15 Day)
             [ "15 дни"
             , "петнадесет дни"
             , "половин месец"
             ]
  , examples (DurationData 7 Week)
             [ "7 седмици"
             , "седем седмици"
             ]
  , examples (DurationData 1 Month)
             [ "1 месец"
             , "месец"
             ]
  , examples (DurationData 6 Month)
             [ "6 месеца"
             , "шест месеца"
             , "половин година"
             ]
  , examples (DurationData 9072000 Second)
             [ "3.5 месеца"
             , "приблизително 3.5 месеца"
             ]
  , examples (DurationData 30 Month)
             [ "две години и половина"
             , "2 години и половина"
             ]
  , examples (DurationData 3 Quarter)
             [ "3 тримесечия"
             ]
  , examples (DurationData 2 Year)
             [ "2 години"
             , "две години"
             ]
  , examples (DurationData 12 Hour)
             [ "12 часа"
             , "дванадесет часа"
             ]
  ]
