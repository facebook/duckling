-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Distance.CS.Corpus
  ( corpus ) where

import Data.String
import Prelude

import Duckling.Distance.Types
import Duckling.Lang
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {lang = CS}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (DistanceValue Kilometre 3)
             [ "3 kilometry"
             , "3 km"
             , "3km"
             ]
  , examples (DistanceValue Mile 8)
             [ "8 mil"
             , "osm mil"
             ]
  , examples (DistanceValue Metre 1)
             [ "1m"
             , "1 metr"
             ]
  , examples (DistanceValue Metre 2)
             [ "2m"
             , "2 metry"
             ]
  , examples (DistanceValue Metre 9)
             [ "9m"
             , "9 metrů"
             ]
  , examples (DistanceValue Centimetre 1)
             [ "1cm"
             , "1 centimetr"
             ]
  , examples (DistanceValue Centimetre 2)
             [ "2cm"
             , "2 centimetry"
             ]
  , examples (DistanceValue Centimetre 9)
             [ "9cm"
             , "9 centimetrů"
             ]
  ]
