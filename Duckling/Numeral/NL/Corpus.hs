-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.NL.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Lang
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {lang = NL}, allExamples)

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
  , examples (NumeralValue 122)
             [ "honderd tweeëntwintig"
             , "honderd tweeentwintig"
             , "honderd twee en twintig"
             ]
  , examples (NumeralValue 20000)
             [ "twintig duizend"
             ]
  ]
