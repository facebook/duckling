-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Quantity.FR.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Lang
import Duckling.Quantity.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {lang = FR}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (QuantityData Cup 2 (Just "café"))
             [ "2 tasses de café"
             ]
  , examples (QuantityData Cup 1 Nothing)
             [ "une Tasse"
             ]
  , examples (QuantityData Tablespoon 3 (Just "sucre"))
             [ "3 Cuillères à soupe de sucre"
             ]
  ]
