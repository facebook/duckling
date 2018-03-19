-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Volume.RO.Corpus
  ( corpus ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Resolve
import Duckling.Volume.Types
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale RO Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (VolumeValue Millilitre 250)
             [ "250 mililitri"
             , "250ml"
             , "250 ml"
             ]
  , examples (VolumeValue Litre 2)
             [ "2 litri"
             , "2 l"
             , "2l"
             ]
  , examples (VolumeValue Gallon 3)
             [ "3 galoane"
             , "3 gal"
             ]
  , examples (VolumeValue Hectolitre 3)
             [ "3 hectolitri"
             ]
  , examples (VolumeValue Litre 0.5)
             [ "jumatate de litru"
             , "jumătate de litru"
             ]
  ]
