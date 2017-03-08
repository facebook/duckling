-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Number.RU.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Lang
import Duckling.Number.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {lang = RU}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumberValue 0)
             [ "0"
             , "ноль"
             ]
  , examples (NumberValue 1)
             [ "1"
             , "один"
             ]
  , examples (NumberValue 2)
             [ "2"
             , "02"
             , "два"
             ]
  , examples (NumberValue 3)
             [ "3"
             , "три"
             , "03"
             ]
  , examples (NumberValue 4)
             [ "4"
             , "четыре"
             , "04"
             ]
  , examples (NumberValue 5)
             [ "пять"
             , "5"
             , "05"
             ]
  , examples (NumberValue 33)
             [ "33"
             , "тридцать три"
             , "0033"
             ]
  , examples (NumberValue 14)
             [ "14"
             , "четырнадцать"
             ]
  , examples (NumberValue 16)
             [ "16"
             , "шестнадцать"
             ]
  , examples (NumberValue 17)
             [ "17"
             , "семнадцать"
             ]
  , examples (NumberValue 18)
             [ "18"
             , "восемнадцать"
             ]
  , examples (NumberValue 525)
             [ "пятьсот двадцать пять"
             , "525"
             ]
  , examples (NumberValue 1.1)
             [ "1.1"
             , "1.10"
             , "01.10"
             , "1 точка 1"
             , "один точка один"
             ]
  , examples (NumberValue 0.77)
             [ "0.77"
             , ".77"
             ]
  , examples (NumberValue 100000)
             [ "100000"
             , "100к"
             , "100К"
             ]
  , examples (NumberValue 3000000)
             [ "3М"
             , "3000К"
             , "3000000"
             ]
  , examples (NumberValue 1200000)
             [ "1200000"
             , "1.2М"
             , "1200К"
             , ".0012Г"
             ]
  , examples (NumberValue (-1200000))
             [ "-1200000"
             , "минус 1200000"
             , "-1.2М"
             , "-1200К"
             , "-.0012Г"
             ]
  ]
