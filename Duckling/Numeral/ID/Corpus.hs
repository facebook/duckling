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
  [ examples (simple 0)
             [ "0"
             , "nol"
             , "kosong"
             ]
  , examples (simple 1)
             [ "1"
             , "satu"
             ]
  , examples (simple 2)
             [ "2"
             , "Dua"
             ]
  , examples (simple 10)
             [ "10"
             , "sepuluh"
             ]
  , examples (simple 33)
             [ "33"
             , "tiga puluh tiga"
             , "0033"
             ]
  , examples (simple 100)
             [ "100"
             , "seratus"
             ]
  , examples (simple 17)
             [ "17"
             , "tujuh belas"
             ]
  , examples (simple 28)
             [ "28"
             , "dua puluh delapan"
             ]
  , examples (simple 1.1)
             [ "1,1"
             , "1,10"
             , "01,10"
             ]
  , examples (simple 0.77)
             [ "0,77"
             , ",77"
             ]
  , examples (simple 100000)
             [ "100.000"
             , "100000"
             , "100K"
             , "100k"
             ]
  , examples (simple 3000000)
             [ "3M"
             , "3000K"
             , "3000000"
             , "3.000.000"
             ]
  , examples (simple 1200000)
             [ "1.200.000"
             , "1200000"
             , "1,2M"
             , "1200K"
             , ",0012G"
             ]
  , examples (simple (-1200000))
             [ "- 1.200.000"
             , "-1200000"
             , "minus 1.200.000"
             , "negatif 1200000"
             , "-1,2M"
             , "-1200K"
             , "-,0012G"
             , "-0,0012G"
             ]
  , examples (simple 5000)
             [ "5 ribu"
             , "lima ribu"
             ]
  , examples (simple 122)
             [ "seratus dua puluh dua"
             ]
  , examples (simple 200000)
             [ "dua ratus ribu"
             ]
  , examples (simple 10011)
             [ "sepuluh ribu sebelas"
             ]
  , examples (simple 721012)
             [ "tujuh ratus dua puluh satu ribu dua belas"
             ]
  , examples (simple 31256721)
             [ "tiga puluh satu juta dua ratus lima puluh enam ribu tujuh ratus dua puluh satu"
             ]
  ]
