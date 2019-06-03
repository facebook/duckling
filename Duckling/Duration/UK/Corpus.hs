-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.UK.Corpus
  ( corpus
  , negativeCorpus
  ) where

import Prelude
import Data.String

import Duckling.Duration.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types
import Duckling.TimeGrain.Types (Grain(..))

context :: Context
context = testContext {locale = makeLocale UK Nothing}

corpus :: Corpus
corpus = (context, testOptions, allExamples)

negativeCorpus :: NegativeCorpus
negativeCorpus = (context, testOptions, examples)
  where
    examples =
      [ "в дні"
      , "секретар"
      , "хвилини"
      ]

allExamples :: [Example]
allExamples = concat
  [ examples (DurationData 1 Second)
             [ "1 секунда"
             , "одна сек"
             ]
  , examples (DurationData 2 Minute)
             [ "2 хв"
             , "дві хвилини"
             ]
  , examples (DurationData 30 Day)
             [ "30 днів"
             ]
  , examples (DurationData 7 Week)
             [ "сім тижнів"
             ]
  , examples (DurationData 1 Month)
             [ "1 місяць"
             ]
  , examples (DurationData 2 Year)
             [ "2 роки"
             ]
  , examples (DurationData 30 Minute)
             [ "півгодини"
             , "1/2 години"
             ]
  , examples (DurationData 12 Hour)
             [ "пів дня"
             ]
  , examples (DurationData 90 Minute)
             [ "півтори години"
             ]
  , examples (DurationData 27 Month)
             [ "2 роки і 3 місяці"
             , "2 роки, 3 місяці"
             ]
  , examples (DurationData 31719604 Second)
             [ "1 рік, 2 дня, 3 години і 4 секунди"
             ]
  ]
