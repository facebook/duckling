-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.CS.Corpus
  ( corpus ) where

import Data.String
import Prelude

import Duckling.Lang
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {lang = CS}, allExamples)

allExamples :: [Example]
allExamples = concat  
  [ examples (NumeralValue 0)
             [ "0"
             , "nula"
             ]
  , examples (NumeralValue 1)
             [ "1"
             , "jeden"
             , "jedna"
             , "jedno"
             ]
  , examples (NumeralValue 2)
             [ "2"
             , "dva"
             , "dvě"
             ]
  , examples (NumeralValue 3)
             [ "3"
             , "tři"
             ]
  , examples (NumeralValue 4)
             [ "4"
             , "čtyři"
             ]
  , examples (NumeralValue 5)
             [ "pět"
             ]
  , examples (NumeralValue 6)
             [ "šest"
             ]
  , examples (NumeralValue 7)
             [ "7"
             , "sedm"
             ]
  , examples (NumeralValue 8)
             [ "osm"
             ]
  , examples (NumeralValue 9)
             [ "devět"
             ]
  , examples (NumeralValue 10)
             [ "deset"
             ]
  , examples (NumeralValue 0.1)
             [ "0,1"
             , "nula celá 1"
             , "žádná celá 1"
             , "jedna desetina"
             ]
  , examples (NumeralValue 0.77)
             [ "0,77"
             , "nula celá sedmesát sedm"
             , "žádná celá sedmesát sedm"
             ]
  , examples (NumeralValue 1.1)
             [ "1,1"
             , "1,10"
             , "1 celá 1"
             ]
  ]
