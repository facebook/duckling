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
  [ examples (simple 0)
             [ "0"
             , "nolla"
             ]
  , examples (simple 1)
             [ "1"
             , "yksi"
             ]
  , examples (simple 2)
             [ "kaksi"
             ]
  , examples (simple 3)
             [ "kolme"
             ]
  , examples (simple 4)
             [ "neljä"
             ]
  , examples (simple 5)
             [ "viisi"
             ]
  , examples (simple 6)
             [ "kuusi"
             ]
  , examples (simple 7)
             [ "seitsemän"
             ]
  , examples (simple 8)
             [ "kahdeksan"
             ]
  , examples (simple 9)
             [ "yhdeksän"
             ]
  , examples (simple 10)
             [ "10"
             , "kymmenen"
             ]
  , examples (simple 11)
             [ "yksitoista"
             ]
  , examples (simple 12)
             [ "kaksitoista"
             ]
  , examples (simple 13)
             [ "kolmetoista"
             ]
  , examples (simple 14)
             [ "neljätoista"
             ]
  , examples (simple 15)
             [ "viisitoista"
             ]
  , examples (simple 16)
             [ "kuusitoista"
             ]
  , examples (simple 17)
             [ "seitsemäntoista"
             ]
  , examples (simple 18)
             [ "kahdeksantoista"
             ]
  , examples (simple 19)
             [ "yhdeksäntoista"
             ]
  , examples (simple 20)
             [ "20"
             , "kaksikymmentä"
             ]
  , examples (simple 24)
             [ "24"
             , "kaksikymmentäneljä"
             ]
  , examples (simple 35)
             [ "kolmekymmentäviisi"
             ]
  , examples (simple 42)
             [ "neljäkymmentäkaksi"
             ]
  , examples (simple 52)
             [ "viisikymmentäkaksi"
             ]
  , examples (simple 62)
             [ "kuusikymmentäkaksi"
             ]
  , examples (simple 72)
             [ "seitsemänkymmentäkaksi"
             ]
  , examples (simple 82)
             [ "kahdeksankymmentäkaksi"
             ]
  , examples (simple 99)
             [ "99"
             , "yhdeksänkymmentäyhdeksän"
             ]
  ]
