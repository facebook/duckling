-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Distance.AR.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Distance.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale AR Nothing}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (DistanceValue Kilometre 3)
             [ "3 كيلومتر"
             , "3 كيلومترات"
             , "3 كم"
             , "3كم"
             ]
  , examples (DistanceValue Kilometre 3.0)
             [ "3,0 كم"
             ]
  , examples (DistanceValue Mile 8)
             [ "8 أميال"
             , "8 ميل"
             ]
  , examples (DistanceValue Metre 9)
             [ "9 أمتار"
             , "9 متر"
             , "9م"
             ]
  , examples (DistanceValue Centimetre 2)
             [ "2سم"
             , "2 سنتيمتر"
             , "سنتيمتران"
             , "سنتيمترين"
             ]
