-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.IS.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

context :: Context
context = testContext{locale = makeLocale IS Nothing}

corpus :: Corpus
corpus = (context, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumeralValue 0)
             [ "0"
             , "núll"
             , "null"
             ]
  , examples (NumeralValue 1)
             [ "1"
             , "einn"
             ]
  , examples (NumeralValue 2)
             [ "tveir"
             ]
  , examples (NumeralValue 3)
             [ "þrír"
             ]
  , examples (NumeralValue 4)
             [ "fjórir"
             ]
  , examples (NumeralValue 5)
             [ "fimm"
             ]
  , examples (NumeralValue 6)
             [ "sex"
             ]
  , examples (NumeralValue 7)
             [ "sjö"
             ]
  , examples (NumeralValue 8)
             [ "átta"
             ]
  , examples (NumeralValue 9)
             [ "níu"
             ]
  , examples (NumeralValue 10)
             [ "tíu"
             ]
  , examples (NumeralValue 11)
             [ "ellefu"
             ]
  , examples (NumeralValue 15)
             [ "fimmtán"
             ]
  , examples (NumeralValue 17)
             [ "sautján"
             ]
  , examples (NumeralValue 20)
             [ "20"
             , "tuttugu"
             ]
  ]
