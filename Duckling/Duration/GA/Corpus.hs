-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.GA.Corpus
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
corpus = (testContext {locale = makeLocale GA Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (DurationData 1 Second)
             [ "aon soicind amhain"
             , "aon soicind"
             , "1 tshoicindi"
             , "1 tsoicind"
             ]
  , examples (DurationData 30 Minute)
             [ "leathuair"
             , "30 noimead"
             ]
  , examples (DurationData 27 Minute)
             [ "7 noimead 20"
             ]
  , examples (DurationData 14 Day)
             [ "coicís"
             ]
  ]
