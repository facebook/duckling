-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.ID.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale ID Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumeralValue 0)
             [ "0"
             , "nol"
             , "kosong"
             ]
  , examples (NumeralValue 1)
             [ "1"
             , "satu"
             ]
  , examples (NumeralValue 2)
             [ "2"
             , "Dua"
             ]
  , examples (NumeralValue 10)
             [ "10"
             , "sepuluh"
             ]
  , examples (NumeralValue 33)
             [ "33"
             , "tiga puluh tiga"
             , "0033"
             ]
  , examples (NumeralValue 100)
             [ "100"
             , "seratus"
             ]
  , examples (NumeralValue 17)
             [ "17"
             , "tujuh belas"
             ]
  , examples (NumeralValue 28)
             [ "28"
             , "dua puluh delapan"
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
  , examples (NumeralValue 100000)
             [ "100.000"
             , "100000"
             , "100K"
             , "100k"
             ]
  , examples (NumeralValue 3000000)
             [ "3M"
             , "3000K"
             , "3000000"
             , "3.000.000"
             ]
  , examples (NumeralValue 1200000)
             [ "1.200.000"
             , "1200000"
             , "1,2M"
             , "1200K"
             , ",0012G"
             ]
  , examples (NumeralValue (-1200000))
             [ "- 1.200.000"
             , "-1200000"
             , "minus 1.200.000"
             , "negatif 1200000"
             , "-1,2M"
             , "-1200K"
             , "-,0012G"
             , "-0,0012G"
             ]
  , examples (NumeralValue 5000)
             [ "5 ribu"
             , "lima ribu"
             ]
  , examples (NumeralValue 122)
             [ "seratus dua puluh dua"
             ]
  , examples (NumeralValue 200000)
             [ "dua ratus ribu"
             ]
  , examples (NumeralValue 10011)
             [ "sepuluh ribu sebelas"
             ]
  , examples (NumeralValue 721012)
             [ "tujuh ratus dua puluh satu ribu dua belas"
             ]
  , examples (NumeralValue 31256721)
             [ "tiga puluh satu juta dua ratus lima puluh enam ribu tujuh ratus dua puluh satu"
             ]
  ]
