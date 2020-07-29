-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.RU.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale RU Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple 0)
             [ "0"
             , "ноль"
             , "нисколько"
             ]
  , examples (simple 1)
             [ "1"
             , "один"
             ]
  , examples (simple 2)
             [ "2"
             , "02"
             , "два"
             ]
  , examples (simple 3)
             [ "3"
             , "три"
             , "03"
             ]
  , examples (simple 4)
             [ "4"
             , "четыре"
             , "04"
             ]
  , examples (simple 5)
             [ "пять"
             , "5"
             , "05"
             ]
  , examples (simple 33)
             [ "33"
             , "тридцать три"
             , "0033"
             ]
  , examples (simple 11)
             [ "11"
             , "одиннадцать"
             ]
  , examples (simple 14)
             [ "14"
             , "четырнадцать"
             ]
  , examples (simple 16)
             [ "16"
             , "шестнадцать"
             ]
  , examples (simple 17)
             [ "17"
             , "семнадцать"
             ]
  , examples (simple 18)
             [ "18"
             , "восемнадцать"
             ]
  , examples (simple 312)
             [ "триста двенадцать"
             , "312"
             ]
  , examples (simple 444)
             [ "четыреста сорок четыре"
             , "444"
             ]
  , examples (simple 525)
             [ "пятьсот двадцать пять"
             , "525"
             ]
  , examples (simple 1.5)
             [ "1.5"
             , "полторы"
             , "один с половиной"
             ]
  , examples (simple 3.5)
             [ "3.5"
             , "три с половиной"
             ]
  , examples (simple 1.1)
             [ "1.1"
             , "1.10"
             , "01.10"
             , "1 точка 1"
             , "один точка один"
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
