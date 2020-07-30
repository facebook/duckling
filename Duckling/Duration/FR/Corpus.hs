-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.FR.Corpus
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

corpus :: Corpus
corpus = (testContext {locale = makeLocale FR Nothing}, testOptions, allExamples)

negativeCorpus :: NegativeCorpus
negativeCorpus = (testContext {locale = makeLocale FR Nothing}, testOptions, examples)
  where
    examples =
      [ "les jours"
      , "en secondaire"
      , "minutes"
      , "pendant des mois"
      ]

allExamples :: [Example]
allExamples = concat
  [ examples (single 1 Second)
             [ "une sec"
             , "1 seconde"
             , "1\""
             ]
  , examples (single 2 Minute)
             [ "2 mins"
             , "deux minutes"
             , "2'"
             ]
  , examples (single 30 Day)
             [ "30 jours"
             ]
  , examples (single 7 Week)
             [ "sept semaines"
             ]
  , examples (single 1 Month)
             [ "1 mois"
             , "un mois"
             ]
  , examples (single 3 Quarter)
             [ "3 trimestres"
             ]
  , examples (single 2 Year)
             [ "2 ans"
             ]
  ]
