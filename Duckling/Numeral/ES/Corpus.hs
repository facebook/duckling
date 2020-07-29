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
    [ examples (simple 1) ["1", "uno", "una"]
    , examples (simple 11) ["once"]
    , examples
        (simple 16)
        ["dieciséis", "dieciseis", "Diesiseis", "diez y seis"]
    , examples (simple 21) ["veintiuno", "veinte y uno"]
    , examples (simple 22) ["veintidós"]
    , examples (simple 23) ["veintitrés", "veinte y tres"]
    , examples (simple 70) ["setenta"]
    , examples (simple 78) ["Setenta y ocho"]
    , examples (simple 80) ["ochenta"]
    , examples (simple 33) ["33", "treinta y tres", "treinta y 3"]
    , examples (simple 100000) ["100000", "100K", "100k"]
    , examples (simple 300) ["trescientos"]
    , examples (simple 243) ["243"]
    , examples (simple 3000000) ["3M", "3000K", "3000000"]
    , examples (simple 1200000) ["1200000", "1200K"]
    , examples (simple (-1200000)) ["-1200000", "-1200K"]
    , examples (simple 1.5) ["1 punto cinco", "una punto cinco"]
    , examples (simple 1) ["cero uno", "zero uno"]
    , examples (simple 2) ["cero dos", "zero dos"]
    , examples (simple 3)
               [
                 "cero tres",
                 "cero trés",
                 "zero tres",
                 "zero trés"
               ]
    , examples (simple 4) ["cero cuatro", "zero cuatro"]
    , examples (simple 5) ["cero cinco", "zero cinco"]
    , examples (simple 6)
               [
                 "cero seis",
                 "cero séis",
                 "zero seis",
                 "zero séis"
               ]
    , examples (simple 7) ["cero siete", "zero siete"]
    , examples (simple 8) ["cero ocho", "zero ocho"]
    , examples (simple 9) ["cero nueve", "zero nueve"]
    ]
