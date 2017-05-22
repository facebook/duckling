-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.DE.Corpus
  ( corpus
  , negativeCorpus
  ) where

import Prelude
import Data.String

import Duckling.Lang
import Duckling.Ordinal.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {lang = DE}, allExamples)

negativeCorpus :: NegativeCorpus
negativeCorpus = (testContext {lang = DE}, examples)
  where
    examples =
      [ "1.1"
      , "1.1."
      ]

allExamples :: [Example]
allExamples =
  examples (OrdinalData 4)
           [ "vierter"
           , "4ter"
           , "Vierter"
           , "4."
           ]
