-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}
module Duckling.Numeral.CA.Corpus (corpus) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus =
  (testContext { locale = makeLocale ES Nothing }, testOptions, allExamples)

allExamples :: [Example]
allExamples =
  concat
    [ examples (NumeralValue 1) ["1", "u", "una"]
    , examples (NumeralValue 11) ["onze"]
    , examples (NumeralValue 12) ["dotze"]
    , examples (NumeralValue 13) ["tretze"]
    , examples (NumeralValue 14) ["catorze"]
    , examples (NumeralValue 15) ["quinze"]
    , examples (NumeralValue 16) ["setze"]
    , examples (NumeralValue 17) ["disset", "diset", "dèsset"]
    , examples (NumeralValue 18) ["divuit", "dihuit"]
    , examples (NumeralValue 19) ["dinou", "dènou"]
    , examples (NumeralValue 20) ["vint"]
    , examples (NumeralValue 21) ["vint-i-un", "vint i un"]
    , examples (NumeralValue 22) ["vint-i-dos", "vint i dos"]
    , examples (NumeralValue 23) ["vint-i-tres", "vint i tres"]
    , examples (NumeralValue 40) ["quaranta"]
    , examples (NumeralValue 70) ["setenta"]
    , examples (NumeralValue 78) ["Setenta-vuit"]
    , examples (NumeralValue 80) ["ochenta"]
    , examples (NumeralValue 33) ["33", "trenta-tres", "treinta y 3"]
    , examples (NumeralValue 100000) ["100000", "100K", "100k"]
    , examples (NumeralValue 300) ["tres-cents"]
    , examples (NumeralValue 243) ["243", "dos-cents quaranta-tres"]
    , examples (NumeralValue 3000000) ["3M", "3000K", "3000000"]
    , examples (NumeralValue 1200000) ["1200000", "1200K"]
    , examples (NumeralValue (-1200000)) ["-1200000", "-1200K"]
    , examples (NumeralValue 1.5) ["1 coma cinc", "una coma cinc"]
    , examples (NumeralValue 1) ["zero u", "zero un"]
    , examples (NumeralValue 2) ["zero dos"]
    , examples (NumeralValue 3)
               [
                 "zero tres",
                 "zero trés"
               ]
    , examples (NumeralValue 4) ["zero quatre", "zero cuatro"]
    , examples (NumeralValue 5) ["zero cinc"]
    , examples (NumeralValue 6) ["zero sis"]
    , examples (NumeralValue 7) ["zero set"]
    , examples (NumeralValue 8) ["zero vuit"]
    , examples (NumeralValue 9) ["zero nou"]
    ]

-- Ull, revisar la xifra amb decimals