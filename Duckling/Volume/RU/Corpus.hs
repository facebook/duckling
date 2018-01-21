-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Volume.RU.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types
import Duckling.Volume.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale RU Nothing}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (VolumeValue Millilitre 250)
             [ "250 миллилитров"
             , "250мл"
             , "250 мл"
             ]
  , examples (VolumeValue Litre 2)
             [ "2 литра"
             , "2 л"
             , "два литра"
             ]
  , examples (VolumeValue Litre 1)
             [ "1 литр"
             , "один литр"
             , "1л"
             ]
  , examples (VolumeValue Gallon 3)
             [ "3 галлона"
             ]
  , examples (VolumeValue Hectolitre 3)
             [ "3 гектолитра"
             , "3 гл"
             , "3гл"
             ]
  , examples (VolumeValue Litre 0.5)
             [ "пол-литра"
             , "поллитра"
             , "пол литра"
             ]
  , examples (VolumeValue Litre 1.5)
             [ "полтора литра"
             ]
  ]
