-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Number.ID.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Lang
import Duckling.Number.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {lang = ID}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumberValue 0)
             [ "0"
             , "nol"
             , "kosong"
             ]
  , examples (NumberValue 1)
             [ "1"
             , "satu"
             ]
  , examples (NumberValue 2)
             [ "2"
             , "Dua"
             ]
  , examples (NumberValue 10)
             [ "10"
             , "sepuluh"
             ]
  , examples (NumberValue 33)
             [ "33"
             , "tiga puluh tiga"
             , "0033"
             ]
  , examples (NumberValue 100)
             [ "100"
             , "seratus"
             ]
  , examples (NumberValue 17)
             [ "17"
             , "tujuh belas"
             ]
  , examples (NumberValue 28)
             [ "28"
             , "dua puluh delapan"
             ]
  , examples (NumberValue 1.1)
             [ "1,1"
             , "1,10"
             , "01,10"
             ]
  , examples (NumberValue 0.77)
             [ "0,77"
             , ",77"
             ]
  , examples (NumberValue 100000)
             [ "100.000"
             , "100000"
             , "100K"
             , "100k"
             ]
  , examples (NumberValue 3000000)
             [ "3M"
             , "3000K"
             , "3000000"
             , "3.000.000"
             ]
  , examples (NumberValue 1200000)
             [ "1.200.000"
             , "1200000"
             , "1,2M"
             , "1200K"
             , ",0012G"
             ]
  , examples (NumberValue (-1200000))
             [ "- 1.200.000"
             , "-1200000"
             , "minus 1.200.000"
             , "negatif 1200000"
             , "-1,2M"
             , "-1200K"
             , "-,0012G"
             , "-0,0012G"
             ]
  , examples (NumberValue 5000)
             [ "5 ribu"
             , "lima ribu"
             ]
  , examples (NumberValue 122)
             [ "seratus dua puluh dua"
             ]
  , examples (NumberValue 200000)
             [ "dua ratus ribu"
             ]
  , examples (NumberValue 10011)
             [ "sepuluh ribu sebelas"
             ]
  , examples (NumberValue 721012)
             [ "tujuh ratus dua puluh satu ribu dua belas"
             ]
  , examples (NumberValue 31256721)
             [ "tiga puluh satu juta dua ratus lima puluh enam ribu tujuh ratus dua puluh satu"
             ]
  ]
