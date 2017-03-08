-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Quantity.PT.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Lang
import Duckling.Quantity.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {lang = PT}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (QuantityData Cup 2 (Just "café"))
             [ "2 copos de café"
             ]
  , examples (QuantityData Cup 1 Nothing)
             [ "um Copo"
             ]
  , examples (QuantityData Pound 100 (Just "acucar"))
             [ "100 Libras de acucar"
             ]
  ]
