-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.NL.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale NL Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumeralValue 0)
             [ "0"
             , "nul"
             , "geen"
             , "niks"
             ]
  , examples (NumeralValue 1)
             [ "1"
             , "een"
             , "één"
             ]
  , examples (NumeralValue 2)
             [ "2"
             , "twee"
             ]
  , examples (NumeralValue 33)
             [ "33"
             , "3 en 30"
             , "drieendertig"
             , "drieëndertig"
             , "drie en dertig"
             , "0033"
             ]
  , examples (NumeralValue 12)
             [ "twaalf"
             , "dozijn"
             ]
  , examples (NumeralValue 14)
             [ "14"
             , "veertien"
             ]
  , examples (NumeralValue 16)
             [ "16"
             , "zestien"
             ]
  , examples (NumeralValue 17)
             [ "17"
             , "zeventien"
             ]
  , examples (NumeralValue 18)
             [ "18"
             , "achtien"
             ]
  , examples (NumeralValue 1.1)
             [ "1,1"
             , "1,10"
             , "01,10"
             ]
  , examples (NumeralValue 0.77)
             [ "0,77"
             , ",77"
             ]
  , examples (NumeralValue 300)
             [ "3 honderd"
             , "drie honderd"
             ]
  , examples (NumeralValue 5000)
             [ "5 duizend"
             , "vijf duizend"
             ]
  , examples (NumeralValue 144)
             [ "gros"
             ]
  , examples (NumeralValue 122)
             [ "honderd tweeëntwintig"
             , "honderd tweeentwintig"
             , "honderd twee en twintig"
             ]
  , examples (NumeralValue 20000)
             [ "twintig duizend"
             ]
  , examples (NumeralValue 0.2)
             [ "1/5"
             , "2/10"
             , "3/15"
             , "20/100"
             ]
  ]
