-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Quantity.KO.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Lang
import Duckling.Quantity.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {lang = KO}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (QuantityData (Custom "근") 2 (Just "삼겹살"))
             [ "삼겹살 두근"
             ]
  , examples (QuantityData (Custom "근") 1 Nothing)
             [ "한근"
             ]
  , examples (QuantityData Gram 600 Nothing)
             [ "육백그람"
             ]
  , examples (QuantityData Cup 3 (Just "콜라"))
             [ "콜라 세컵"
             ]
  ]
