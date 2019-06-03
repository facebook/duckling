-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.KA.Corpus
  ( corpus
  ) where

import Prelude
import Data.String

import Duckling.Duration.Types
import Duckling.Testing.Types
import Duckling.TimeGrain.Types (Grain(..))

import Duckling.Locale
import Duckling.Resolve

corpus :: Corpus
corpus = (testContext {locale = makeLocale KA Nothing},
  testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (DurationData 1 Second)
             [ "ერთი წამი"
             , "1 წამი"
             ]
  , examples (DurationData 2 Minute)
             [ "2 წუთი"
             , "ორი წუთი"
             ]
  , examples (DurationData 30 Day)
             [ "30 დღე"
             ]
  , examples (DurationData 7 Week)
             [ "შვიდი კვირა"
             ]
  , examples (DurationData 1 Month)
             [ "1 თვე"
             , "ერთი თვე"
             ]
  , examples (DurationData 3 Quarter)
             [ "3 კვარტალი"
             ]
  , examples (DurationData 27 Month)
             [ "2 წელი და 3 თვე"
             , "2 წელი, 3 თვე"
             ]
  , examples (DurationData 31719604 Second)
             [ "1 წელი, 2 დღე, 3 საათი და 4 წამი"
             ]
  ]
