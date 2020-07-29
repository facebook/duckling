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
  [ examples (simple 0)
             [ "0"
             , "núll"
             , "null"
             ]
  , examples (simple 1)
             [ "1"
             , "einn"
             ]
  , examples (simple 2)
             [ "tveir"
             ]
  , examples (simple 3)
             [ "þrír"
             ]
  , examples (simple 4)
             [ "fjórir"
             ]
  , examples (simple 5)
             [ "fimm"
             ]
  , examples (simple 6)
             [ "sex"
             ]
  , examples (simple 7)
             [ "sjö"
             ]
  , examples (simple 8)
             [ "átta"
             ]
  , examples (simple 9)
             [ "níu"
             ]
  , examples (simple 10)
             [ "tíu"
             ]
  , examples (simple 11)
             [ "ellefu"
             ]
  , examples (simple 15)
             [ "fimmtán"
             ]
  , examples (simple 17)
             [ "sautján"
             ]
  , examples (simple 20)
             [ "20"
             , "tuttugu"
             ]
  ]
