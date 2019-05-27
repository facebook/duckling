-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.SV.Corpus
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
corpus = (testContext {locale = makeLocale SV Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (DurationData 1 Second)
             [ "enkel sek"
             , "1 sekund fler"
             , "1 sekunder mer"
             , "en sekunderna"
             , "et sek"
             , "ett sek"
             ]
  , examples (DurationData 30 Minute)
             [ "1/2 timme"
             , "en halv timme"
             , "0,5 timmar"
             , "30 minuterna"
             , "trettio min"
             ]
  , examples (DurationData 5 Year)
             [ "5 år"
             , "fem år"
             , "omkring fem år"
             , "ungefär fem år"
             , "runt fem år"
             , "cirka fem år"
             , "ca fem år"
             , "ca. fem år"
             , "c:a fem år"
             ]
  ]
