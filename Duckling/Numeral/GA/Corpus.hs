-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.GA.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale GA Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumeralValue 0)
             [ "0"
             , "a náid"
             ]
  , examples (NumeralValue 1)
             [ "1"
             , "aon"
             , "a haon"
             , "Amháin"
             ]
  , examples (NumeralValue 20)
             [ "20"
             , "Fiche"
             ]
  , examples (NumeralValue 30)
             [ "déag is fiche"
             ]
  , examples (NumeralValue 40)
             [ "is dha fhichead"
             , "is dá fhichead"
             ]
  , examples (NumeralValue 50)
             [ "deag is dha fhichead"
             , "deag is dá fhichead"
             ]
  ]
