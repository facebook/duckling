-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.NB.Corpus
  ( corpus
  ) where

import Prelude
import Data.String

import Duckling.Duration.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types
import Duckling.TimeGrain.Types (Grain(..))

corpus :: Corpus
corpus = (testContext {locale = makeLocale NB Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (DurationData 1 Second)
             [ "1 sek"
             , "en sek"
             , "ett sekund"
             , "e sekunder"
             ]
  , examples (DurationData 30 Minute)
             [ "tredve min"
             , "30 minutt"
             , "30 minutter"
             , "1/2 time"
             , "en halv time"
             ]
  , examples (DurationData 2 Day)
             [ "et par dager"
             , "2 dag"
             , "to dag"
             ]
  ]
