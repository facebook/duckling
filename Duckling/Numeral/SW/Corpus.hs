-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.SW.Corpus
  ( corpus ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale SW Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumeralValue 0)
             [ "0"
             , "sufuri"
             , "zero"
             ]
  , examples (NumeralValue 1)
             [ "moja"
             ]
  , examples (NumeralValue 2)
             [ "mbili"
             ]
  , examples (NumeralValue 3)
             [ "tatu"
             ]
  , examples (NumeralValue 4)
             [ "nne"
             ]
  , examples (NumeralValue 5)
             [ "tano"
             ]
  , examples (NumeralValue 6)
             [ "sita"
             ]
  , examples (NumeralValue 7)
             [ "saba"
             ]
  , examples (NumeralValue 8)
             [ "nane"
             ]
  , examples (NumeralValue 9)
             [ "tisa"
             ]
  , examples (NumeralValue 10)
             [ "kumi"
             ]
  , examples (NumeralValue 20)
             [ "ishirini"
             ]
  , examples (NumeralValue 30)
             [ "thelathini"
             ]
  , examples (NumeralValue 40)
             [ "arubaini"
             , "arobaini"
             ]
  , examples (NumeralValue 50)
             [ "hamsini"
             ]
  , examples (NumeralValue 60)
             [ "sitini"
             ]
  , examples (NumeralValue 70)
             [ "sabini"
             ]
  , examples (NumeralValue 80)
             [ "themanini"
             ]
  , examples (NumeralValue 90)
             [ "tisini"
             ]
  ]
