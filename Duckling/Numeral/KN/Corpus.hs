-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.KN.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale KN Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple 0)
             [ "ಸೊನ್ನೆ"
             , "೦"
             ]
  , examples (simple 1)
             [ "ಒಂದು"
             , "೧"
             ]
  , examples (simple 2)
             [ "ಎರಡು"
             , "೨"
             ]
  , examples (simple 3)
             [ "ಮೂರು"
             , "೩"
             ]
  , examples (simple 4)
             [ "ನಾಲ್ಕು"
             , "೪"
             ]
  , examples (simple 5)
             [ "ಐದು"
             , "೫"
             ]
  , examples (simple 6)
             [ "ಆರು"
             , "೬"
             ]
  , examples (simple 7)
             [ "ಏಳು"
             , "೭"
             ]
  , examples (simple 8)
             [ "ಎಂಟು"
             , "೮"
             ]
  , examples (simple 9)
             [ "ಒಂಬತ್ತು"
             , "೯"
             ]
  ]
