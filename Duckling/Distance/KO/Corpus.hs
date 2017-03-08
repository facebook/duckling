-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Distance.KO.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Distance.Types
import Duckling.Lang
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {lang = KO}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (DistanceValue Kilometre 3)
             [ "3 킬로미터"
             , "3 킬로"
             , "3 키로"
             , "3 km"
             , "3km"
             ]
  , examples (DistanceValue Kilometre 3.0)
             [ "3.0 km"
             ]
  , examples (DistanceValue Mile 8)
             [ "8 miles"
             , "8 마일"
             , "8 마일즈"
             ]
  , examples (DistanceValue Metre 9)
             [ "9m"
             , "9미터"
             , "9메터"
             , "구메터"
             ]
  , examples (DistanceValue Centimetre 2)
             [ "2cm"
             , "2 센치"
             , "이센치"
             , "2 센티"
             , "2 센티미터"
             , "2 센치미터"
             ]
  ]
