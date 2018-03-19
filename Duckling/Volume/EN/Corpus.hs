-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Volume.EN.Corpus
  ( corpus ) where

import Data.String
import Prelude

import Duckling.Testing.Types
import Duckling.Volume.Types

corpus :: Corpus
corpus = (testContext, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (VolumeValue Millilitre 250)
             [ "250 milliliters"
             , "250ml"
             , "250 ml"
             ]
  , examples (VolumeValue Litre 2)
             [ "2 liters" ]
  , examples (VolumeValue Gallon 3)
             [ "3 gallons"
             , "3 gal"
             ]
  , examples (VolumeValue Hectolitre 3)
             [ "3 hectoliters"
             ]
  , examples (VolumeValue Litre 0.5)
             [ "half liter"
             ]
  ]
