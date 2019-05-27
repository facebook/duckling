-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Quantity.KO.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Locale
import Duckling.Quantity.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale KO Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple (Custom "근") 2 (Just "삼겹살"))
             [ "삼겹살 두근"
             ]
  , examples (simple (Custom "근") 1 Nothing)
             [ "한근"
             ]
  , examples (simple Gram 600 Nothing)
             [ "육백그람"
             ]
  , examples (simple Cup 3 (Just "콜라"))
             [ "콜라 세컵"
             ]
  ]
