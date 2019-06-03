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
  [ examples (NumeralValue 0)
             [ "0"
             , "нуль"
             ]
  , examples (NumeralValue 1)
             [ "1"
             , "один"
             ]
  , examples (NumeralValue 2)
             [ "2"
             , "02"
             , "два"
             ]
  , examples (NumeralValue 3)
             [ "3"
             , "три"
             , "03"
             ]
  , examples (NumeralValue 4)
             [ "4"
             , "чотири"
             , "04"
             ]
  , examples (NumeralValue 5)
             [ "п‘ять"
             , "5"
             , "05"
             ]
  , examples (NumeralValue 33)
             [ "33"
             , "тридцять три"
             , "0033"
             ]
  , examples (NumeralValue 14)
             [ "14"
             , "чотирнадцять"
             ]
  , examples (NumeralValue 16)
             [ "16"
             , "шістнадцять"
             ]
  , examples (NumeralValue 17)
             [ "17"
             , "сімнадцять"
             ]
  , examples (NumeralValue 18)
             [ "18"
             , "вісімнадцять"
             ]
  , examples (NumeralValue 525)
             [ "п‘ятсот двадцять п‘ять"
             , "525"
             ]
  , examples (NumeralValue 1.1)
             [ "1.1"
             , "1.10"
             , "01.10"
             , "1 крапка 1"
             , "один крапка один"
             ]
  , examples (NumeralValue 0.77)
             [ "0.77"
             , ".77"
             ]
  , examples (NumeralValue 100000)
             [ "100000"
             , "100к"
             , "100К"
             ]
  , examples (NumeralValue 3000000)
             [ "3М"
             , "3000К"
             , "3000000"
             ]
  , examples (NumeralValue 1200000)
             [ "1200000"
             , "1.2М"
             , "1200К"
             , ".0012Г"
             ]
  , examples (NumeralValue (-1200000))
             [ "-1200000"
             , "мінус 1200000"
             , "-1.2М"
             , "-1200К"
             , "-.0012Г"
             ]
  ]
