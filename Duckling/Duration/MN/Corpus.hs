-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


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
  [ examples (single 1 Second)
             [ "1 сек"
             , "1 секунд"
             , "секунд"
             , "1\""
             ]
  , examples (single 15 Minute)
             [ "15 мин"
             , "15'"
             ]
  , examples (single 30 Minute)
             [ "30 минут"
             ]
  , examples (single 5400 Second)
             [ "5400 секунд"
             ]
  , examples (single 8 Hour)
             [ "8 цаг"
             , "8 ц"
             ]
  , examples (single 15 Day)
             [ "15 өдөр"
             ]
  , examples (single 7 Week)
             [ "7 долоо хоног"
             ]
  , examples (single 1 Month)
             [ "1 сар"
             , "сар"
             ]
  , examples (single 6 Month)
             [ "6 сар"
             ]
  , examples (single 2 Year)
             [ "2 жил"
             ]
  , examples (single 12 Hour)
             [ "12 цаг"
             ]
  ]
