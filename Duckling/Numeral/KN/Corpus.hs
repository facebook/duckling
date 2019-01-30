-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


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
             ]
  , examples (NumeralValue 2)
             [ "ಎರಡು"
             ]
  , examples (NumeralValue 3)
             [ "ಮೂರು"
             ]
  , examples (NumeralValue 4)
             [ "ನಾಲ್ಕು"
             ]
  , examples (NumeralValue 5)
             [ "ಅಯ್ದು"
             ]
  , examples (NumeralValue 6)
             [ "ಆರು"
             ]
  , examples (NumeralValue 7)
             [ "ಏಳು"
             ]
  , examples (NumeralValue 8)
             [ "ಎಂಟು"
             ]
  , examples (NumeralValue 9)
             [ "ಒಂಬತ್ತು"
             ]
  ]