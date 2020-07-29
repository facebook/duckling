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
  [ examples (simple 0)
             [ "0"
             , "nul"
             , "geen"
             , "niks"
             ]
  , examples (simple 1)
             [ "1"
             , "een"
             , "één"
             ]
  , examples (simple 2)
             [ "2"
             , "twee"
             ]
  , examples (simple 33)
             [ "33"
             , "3 en 30"
             , "drieendertig"
             , "drieëndertig"
             , "drie en dertig"
             , "0033"
             ]
  , examples (simple 12)
             [ "twaalf"
             , "dozijn"
             ]
  , examples (simple 14)
             [ "14"
             , "veertien"
             ]
  , examples (simple 16)
             [ "16"
             , "zestien"
             ]
  , examples (simple 17)
             [ "17"
             , "zeventien"
             ]
  , examples (simple 18)
             [ "18"
             , "achtien"
             ]
  , examples (simple 1.1)
             [ "1,1"
             , "1,10"
             , "01,10"
             ]
  , examples (simple 0.77)
             [ "0,77"
             , ",77"
             ]
  , examples (simple 300)
             [ "3 honderd"
             , "drie honderd"
             ]
  , examples (simple 5000)
             [ "5 duizend"
             , "vijf duizend"
             ]
  , examples (simple 144)
             [ "gros"
             ]
  , examples (simple 122)
             [ "honderd tweeëntwintig"
             , "honderd tweeentwintig"
             , "honderd twee en twintig"
             ]
  , examples (simple 20000)
             [ "twintig duizend"
             ]
  , examples (simple 0.2)
             [ "1/5"
             , "2/10"
             , "3/15"
             , "20/100"
             ]
  ]
