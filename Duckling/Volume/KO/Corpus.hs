-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Volume.KO.Corpus
  ( corpus ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Resolve
import Duckling.Volume.Types
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale KO Nothing}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (VolumeValue Millilitre 250)
             [ "250 밀리리터"
             , "250 미리리터"
             , "이백오십미리리터"
             , "250ml"
             , "250 ml"
             ]
  , examples (VolumeValue Litre 2)
             [ "2 리터"
             , "이리터"
             ]
  , examples (VolumeValue Gallon 3)
             [ "3 갤론"
             , "삼 갤론"
             ]
  , examples (VolumeValue Hectolitre 3)
             [ "3 헥토리터"
             , "삼 헥토리터"
             ]
  , examples (VolumeValue Litre 0.5)
             [ "반 리터"
             ]
  ]
