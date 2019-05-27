-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Quantity.FR.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Locale
import Duckling.Quantity.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale FR Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Cup 2 (Just "café"))
             [ "2 tasses de café"
             ]
  , examples (simple Cup 1 Nothing)
             [ "une Tasse"
             ]
  , examples (simple Tablespoon 3 (Just "sucre"))
             [ "3 Cuillères à soupe de sucre"
             ]
  ]
