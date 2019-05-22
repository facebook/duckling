-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.

{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.FI.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

context :: Context
context = testContext {locale = makeLocale FI Nothing}

corpus :: Corpus
corpus = (context, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumeralValue 0)
             [ "0"
             , "nolla"
             ]
  , examples (NumeralValue 1)
             [ "1"
             , "yksi"
             ]
  , examples (NumeralValue 2)
             [ "kaksi"
             ]
  , examples (NumeralValue 3)
             [ "kolme"
             ]
  , examples (NumeralValue 4)
             [ "neljä"
             ]
  , examples (NumeralValue 5)
             [ "viisi"
             ]
  , examples (NumeralValue 6)
             [ "kuusi"
             ]
  , examples (NumeralValue 7)
             [ "seitsemän"
             ]
  , examples (NumeralValue 8)
             [ "kahdeksan"
             ]
  , examples (NumeralValue 9)
             [ "yhdeksän"
             ]
  , examples (NumeralValue 10)
             [ "10"
             , "kymmenen"
             ]
  , examples (NumeralValue 11)
             [ "yksitoista"
             ]
  , examples (NumeralValue 12)
             [ "kaksitoista"
             ]
  , examples (NumeralValue 13)
             [ "kolmetoista"
             ]
  , examples (NumeralValue 14)
             [ "neljätoista"
             ]
  , examples (NumeralValue 15)
             [ "viisitoista"
             ]
  , examples (NumeralValue 16)
             [ "kuusitoista"
             ]
  , examples (NumeralValue 17)
             [ "seitsemäntoista"
             ]
  , examples (NumeralValue 18)
             [ "kahdeksantoista"
             ]
  , examples (NumeralValue 19)
             [ "yhdeksäntoista"
             ]
  , examples (NumeralValue 20)
             [ "20"
             , "kaksikymmentä"
             ]
  , examples (NumeralValue 24)
             [ "24"
             , "kaksikymmentäneljä"
             ]
  , examples (NumeralValue 35)
             [ "kolmekymmentäviisi"
             ]
  , examples (NumeralValue 42)
             [ "neljäkymmentäkaksi"
             ]
  , examples (NumeralValue 52)
             [ "viisikymmentäkaksi"
             ]
  , examples (NumeralValue 62)
             [ "kuusikymmentäkaksi"
             ]
  , examples (NumeralValue 72)
             [ "seitsemänkymmentäkaksi"
             ]
  , examples (NumeralValue 82)
             [ "kahdeksankymmentäkaksi"
             ]
  , examples (NumeralValue 99)
             [ "99"
             , "yhdeksänkymmentäyhdeksän"
             ]
  ]
