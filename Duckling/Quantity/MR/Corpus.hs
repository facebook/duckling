-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Quantity.MR.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Locale
import Duckling.Quantity.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale MR Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Gram 1000 Nothing)
             [ "१ किलोग्रॅम"
             , "एक किलो"
             , "1 kg"
             ]
  , examples (simple Gram (1000/3) (Just "साखर"))
             [ "एक तृतीयांश किलोग्रॅम साखर"
             ]
  ]
