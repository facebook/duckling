-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.BG.Corpus
  ( corpus ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale BG Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple 0)
             [ "0"
             , "нула"
             ]
  , examples (simple 1)
             [ "1"
             , "един"
             , "една"
             , "едно"
             ]
  , examples (simple 2)
             [ "2"
             , "02"
             , "две"
             , "два"
             ]
  , examples (simple 3)
             [ "3"
             , "03"
             , "три"
             ]
  , examples (simple 4)
             [ "4"
             , "04"
             , "четири"
             ]
  , examples (simple 5)
             [ "5"
             , "05"
             , "пет"
             ]
  , examples (simple 33)
             [ "33"
             , "0033"
             , "тридесет и три"
             ]
  , examples (simple 14)
             [ "14"
             , "четиринадесет"
             , "четиринайсет"
             ]
  , examples (simple 15)
             [ "15"
             , "петнадесет"
             , "петнайсет"
             ]
  , examples (simple 16)
             [ "16"
             , "шестнадесет"
             , "шестнайсет"
             ]
  , examples (simple 17)
             [ "17"
             , "седемнадесет"
             , "седемнайсет"
             ]
  , examples (simple 18)
             [ "18"
             , "осемнадесет"
             , "осемнайсет"
             ]
  , examples (simple 525)
             [ "525"
             , "петстотин двадесет и пет"
             ]
  , examples (simple 1.1)
             [ "1.1"
             , "1.10"
             , "01.10"
             , "1 цяло и 1"
             , "едно цяло и едно"
             ]
  , examples (simple 0.77)
             [ "0.77"
             , ".77"
             ]
  , examples (simple 100000)
             [ "100000"
             , "100к"
             , "100К"
             ]
  , examples (simple 3000000)
             [ "3М"
             , "3000К"
             , "3000000"
             , "3,000,000"
             ]
  , examples (simple 1200000)
             [ "1200000"
             , "1.2М"
             , "1200К"
             , ".0012Г"
             ]
  , examples (simple (-1200000))
             [ "-1200000"
             , "минус 1200000"
             , "-1.2М"
             , "-1200К"
             , "-.0012Г"
             ]
  ]
