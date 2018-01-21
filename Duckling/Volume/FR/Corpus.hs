-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Volume.FR.Corpus
  ( corpus ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Resolve
import Duckling.Volume.Types
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale FR Nothing}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (VolumeValue Millilitre 250)
             [ "250 millilitres"
             , "250ml"
             , "250 ml"
             ]
  , examples (VolumeValue Litre 2)
             [ "2 litres"
             ]
  , examples (VolumeValue Gallon 3)
             [ "3 gallons"
             , "3 gal"
             ]
  , examples (VolumeValue Hectolitre 3)
             [ "3 hectolitres"
             ]
  , examples (VolumeValue Litre 0.5)
             [ "demi-litre"
             , "demi litre"
             ]
  ]
