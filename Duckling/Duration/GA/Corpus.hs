-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.GA.Corpus
  ( corpus
  ) where

import Prelude
import Data.String

import Duckling.Duration.Types
import Duckling.Lang
import Duckling.Resolve
import Duckling.Testing.Types
import Duckling.TimeGrain.Types (Grain(..))

corpus :: Corpus
corpus = (testContext {lang = GA}, allExamples)

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
  , examples (DurationData 14 Day)
             [ "coicís"
             ]
  ]
