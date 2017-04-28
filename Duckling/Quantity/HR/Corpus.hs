-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.

{-# LANGUAGE OverloadedStrings #-}

module Duckling.Quantity.HR.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Lang
import Duckling.Quantity.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {lang = HR}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (QuantityData Gram 2000 (Just "meso"))
             [ "dvije kile mesa"
             , "dva kilograma mesa"
             ]
  , examples (QuantityData Gram 1000 Nothing)
             [ "1 kilograma"
             ]
  ]
