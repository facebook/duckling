-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Distance.KO.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Distance.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale KO Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Kilometre 3)
             [ "3 킬로미터"
             , "3 킬로"
             , "3 키로"
             , "3 km"
             , "3km"
             ]
  , examples (simple Kilometre 3.0)
             [ "3.0 km"
             ]
  , examples (simple Mile 8)
             [ "8 miles"
             , "8 마일"
             , "8 마일즈"
             ]
  , examples (simple Metre 9)
             [ "9m"
             , "9미터"
             , "9메터"
             , "구메터"
             ]
  , examples (simple Centimetre 2)
             [ "2cm"
             , "2 센치"
             , "이센치"
             , "2 센티"
             , "2 센티미터"
             , "2 센치미터"
             ]
  ]
