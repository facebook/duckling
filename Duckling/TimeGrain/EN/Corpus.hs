-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}
module Duckling.TimeGrain.EN.Corpus (corpus) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types
import Duckling.TimeGrain.Types

corpus :: Corpus
corpus = (testContext, testOptions, allExamples)

allExamples :: [Example]
allExamples =
  concat
    [ examples Second ["second", "seconds"]
    , examples Minute ["minute", "minutes"]
    , examples Hour ["hour", "hours"]
    , examples Day ["day", "days"]
    , examples Week ["week", "weeks"]
    , examples Month ["month", "months"]
    , examples Quarter ["quarter", "quarters"]
    , examples Year ["year", "years"]
    ]
