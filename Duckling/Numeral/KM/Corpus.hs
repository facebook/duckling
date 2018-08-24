-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.KM.Corpus
  ( corpus ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

context :: Context
context = testContext {locale = makeLocale KM Nothing}

corpus :: Corpus
corpus = (context, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumeralValue 0)
             [ "0"
             , "០"
             ]
  , examples (NumeralValue 1)
             [ "1"
             , "១"
             ]
  , examples (NumeralValue 2)
             [ "2"
             , "២"
             ]
  , examples (NumeralValue 3)
             [ "3"
             , "៣"
             ]
  , examples (NumeralValue 4)
             [ "4"
             , "៤"
             ]
  , examples (NumeralValue 5)
             [ "5"
             , "៥"
             ]
  , examples (NumeralValue 6)
             [ "6"
             , "៦"
             ]
  , examples (NumeralValue 7)
             [ "7"
             , "៧"
             ]
  , examples (NumeralValue 8)
             [ "8"
             , "៨"
             ]
  , examples (NumeralValue 9)
             [ "9"
             , "៩"
             ]
  ]
