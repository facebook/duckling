-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Volume.TR.Corpus
  ( corpus ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types
import Duckling.Volume.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale TR Nothing}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (VolumeValue Millilitre 250)
             [ "250 mililitre"
             , "250ml"
             , "250 ml"
             ]
  , examples (VolumeValue Litre 2)
             [ "2 litre" ]
  , examples (VolumeValue Gallon 3)
             [ "3 galon"
             , "3 gal"
             ]
  , examples (VolumeValue Hectolitre 3)
             [ "3 hektolitre"
             ]
  , examples (VolumeValue Litre 0.5)
             [ "yarım litre"
             ]
  ]
