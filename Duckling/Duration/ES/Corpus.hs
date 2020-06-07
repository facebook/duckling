-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.ES.Corpus
  ( corpus
  ) where

import Prelude
import Data.String

import Duckling.Duration.Types
import Duckling.Testing.Types
import Duckling.TimeGrain.Types (Grain(..))

corpus :: Corpus
corpus = (testContext, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (DurationData 30 Minute)
             [ "media hora"
             ]
  ]