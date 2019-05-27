-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.PT.Corpus
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
corpus = (testContext {locale = makeLocale PT Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (DurationData 1 Second)
             [ "um segundo"
             , "uma seg"
             ]
  , examples (DurationData 2 Minute)
             [ "duas mins"
             , "dois minutos"
             ]
  , examples (DurationData 20 Day)
             [ "20 dias"
             , "vinte días"
             ]
  ]
