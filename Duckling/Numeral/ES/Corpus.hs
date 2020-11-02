-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}
module Duckling.Numeral.ES.Corpus (corpus) where

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
    [ examples (NumeralValue 1) ["1", "uno", "una"]
    , examples (NumeralValue 11) ["once"]
    , examples
        (NumeralValue 16)
        ["dieciséis", "dieciseis", "Diesiseis", "diez y seis"]
    , examples (NumeralValue 21) ["veintiuno", "veinte y uno"]
    , examples (NumeralValue 22) ["veintidós"]
    , examples (NumeralValue 23) ["veintitrés", "veinte y tres"]
    , examples (NumeralValue 70) ["setenta"]
    , examples (NumeralValue 78) ["Setenta y ocho"]
    , examples (NumeralValue 80) ["ochenta"]
    , examples (NumeralValue 33) ["33", "treinta y tres", "treinta y 3"]
    , examples (NumeralValue 100000) ["100000", "100K", "100k"]
    , examples (NumeralValue 300) ["trescientos"]
    , examples (NumeralValue 243) ["243"]
    , examples (NumeralValue 3000000) ["3M", "3000K", "3000000"]
    , examples (NumeralValue 1200000) ["1200000", "1200K"]
    , examples (NumeralValue (-1200000)) ["-1200000", "-1200K"]
    , examples (NumeralValue 1.5) ["1 punto cinco", "una punto cinco"]
    , examples (NumeralValue 1) ["cero uno", "zero uno"]
    , examples (NumeralValue 2) ["cero dos", "zero dos"]
    , examples (NumeralValue 3)
               [
                 "cero tres",
                 "cero trés",
                 "zero tres",
                 "zero trés"
               ]
    , examples (NumeralValue 4) ["cero cuatro", "zero cuatro"]
    , examples (NumeralValue 5) ["cero cinco", "zero cinco"]
    , examples (NumeralValue 6)
               [
                 "cero seis",
                 "cero séis",
                 "zero seis",
                 "zero séis"
               ]
    , examples (NumeralValue 7) ["cero siete", "zero siete"]
    , examples (NumeralValue 8) ["cero ocho", "zero ocho"]
    , examples (NumeralValue 9) ["cero nueve", "zero nueve"]
    ]
