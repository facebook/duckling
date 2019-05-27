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
  [ examples (NumeralValue 0)
             [ "ಸೊನ್ನೆ"
             , "೦"
             ]
  , examples (NumeralValue 1)
             [ "ಒಂದು"
             , "೧"
             ]
  , examples (NumeralValue 2)
             [ "ಎರಡು"
             , "೨"
             ]
  , examples (NumeralValue 3)
             [ "ಮೂರು"
             , "೩"
             ]
  , examples (NumeralValue 4)
             [ "ನಾಲ್ಕು"
             , "೪"
             ]
  , examples (NumeralValue 5)
             [ "ಐದು"
             , "೫"
             ]
  , examples (NumeralValue 6)
             [ "ಆರು"
             , "೬"
             ]
  , examples (NumeralValue 7)
             [ "ಏಳು"
             , "೭"
             ]
  , examples (NumeralValue 8)
             [ "ಎಂಟು"
             , "೮"
             ]
  , examples (NumeralValue 9)
             [ "ಒಂಬತ್ತು"
             , "೯"
             ]
  ]
