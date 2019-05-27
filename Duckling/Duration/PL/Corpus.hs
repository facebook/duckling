-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.PL.Corpus
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
corpus = (testContext {locale = makeLocale PL Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (DurationData 1 Second)
             [ "1s"
             , "1 sekund"
             , "jeden sekundzie"
             , "pojedynczy sekundach"
             ]
  , examples (DurationData 30 Minute)
             [ "pol godziny"
             , "pół godziny"
             , "30m"
             , "30 minut"
             , "trzydzieści minutami"
             ]
  , examples (DurationData 5 Day)
             [ "pięciu dniach"
             ]
  , examples (DurationData 100 Day)
             [ "sto dzień"
             , "setki dnią"
             ]
  ]
