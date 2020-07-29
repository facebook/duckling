-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.UK.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale UK Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple 0)
             [ "0"
             , "нуль"
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
             , "чотири"
             , "04"
             ]
  , examples (simple 5)
             [ "п‘ять"
             , "5"
             , "05"
             ]
  , examples (simple 33)
             [ "33"
             , "тридцять три"
             , "0033"
             ]
  , examples (simple 14)
             [ "14"
             , "чотирнадцять"
             ]
  , examples (simple 16)
             [ "16"
             , "шістнадцять"
             ]
  , examples (simple 17)
             [ "17"
             , "сімнадцять"
             ]
  , examples (simple 18)
             [ "18"
             , "вісімнадцять"
             ]
  , examples (simple 525)
             [ "п‘ятсот двадцять п‘ять"
             , "525"
             ]
  , examples (simple 1.1)
             [ "1.1"
             , "1.10"
             , "01.10"
             , "1 крапка 1"
             , "один крапка один"
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
             , "мінус 1200000"
             , "-1.2М"
             , "-1200К"
             , "-.0012Г"
             ]
  ]
