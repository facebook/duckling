-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Quantity.KM.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Quantity.Types
import Duckling.Resolve
import Duckling.Testing.Types

context :: Context
context = testContext{locale = makeLocale KM Nothing}

corpus :: Corpus
corpus = (context, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Cup 3 Nothing)
             [ "បីកែវ"
             ]
  , examples (simple Bowl 1 Nothing)
             [ "១ចាន"
             ]
  , examples (simple Pint 15 Nothing)
             [ "ដប់៥ថូ"
             ]
  ]
