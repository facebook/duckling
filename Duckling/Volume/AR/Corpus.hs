-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Volume.AR.Corpus
  ( corpus ) where

import Data.String
import Prelude

import Duckling.Testing.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Volume.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale AR Nothing}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (VolumeValue Millilitre 250)
             [ "250 مل"
             , "250مل"
             , "250 ميليلتر"
             , "250 ملي لتر"
             ]
  , examples (VolumeValue Litre 2)
             [ "2 لتر"
             , "لتران"
             , "لترين"
             ]
  , examples (VolumeValue Gallon 3)
             [ "3 غالون"
             , "3 جالون"
             , "3 غالونات"
             , "3 جالونات"
             ]
  , examples (VolumeValue Hectolitre 3)
             [ "3 هكتوليتر"
             , "3 هكتو ليتر"
             ]
  , examples (VolumeValue Litre 0.5)
             [ "نصف لتر"
             , "نص لتر"
             ]
  , examples (VolumeValue Litre 0.25)
             [ "ربع لتر"
             ]
  , examples (VolumeValue Litre 1.5)
             [ "لتر ونصف"
             , "لتر و نص"
             , "لتر و نصف"
             , "لتر ونص"
             ]
  , examples (VolumeValue Litre 1.25)
             [ "لتر وربع"
             , "لتر و ربع"
             ]
  ]
