-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.PT.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale PT Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple 1)
             [ "1"
             , "um"
             , "Uma"
             ]
  , examples (simple 2)
             [ "2"
             , "dois"
             , "duas"
             , "pares de"
             , "um par de"
             ]
  , examples (simple 3)
             [ "3"
             , "três"
             , "tres"
             ]
  , examples (simple 6)
             [ "6"
             , "seis"
             ]
  , examples (simple 11)
             [ "11"
             , "onze"
             ]
  , examples (simple 12)
             [ "12"
             , "doze"
             , "uma dúzia"
             , "uma duzia"
             , "uma duzias de"
             ]
  , examples (simple 14)
             [ "14"
             , "Catorze"
             , "quatorze"
             ]
  , examples (simple 16)
             [ "16"
             , "dezesseis"
             , "dezasseis"
             ]
  , examples (simple 17)
             [ "17"
             , "dezessete"
             , "dezassete"
             ]
  , examples (simple 18)
             [ "18"
             , "dezoito"
             ]
  , examples (simple 19)
             [ "19"
             , "dezenove"
             , "dezanove"
             ]
  , examples (simple 21)
             [ "21"
             , "vinte e um"
             ]
  , examples (simple 23)
             [ "23"
             , "vinte e tres"
             , "vinte e três"
             ]
  , examples (simple 24)
             [ "24"
             , "vinte e quatro"
             , "duas dúzias"
             , "duas duzias"
             ]
  , examples (simple 50)
             [ "50"
             , "cinquenta"
             , "cinqüenta"
             , "cincoenta"
             ]
  , examples (simple 70)
             [ "setenta"
             ]
  , examples (simple 78)
             [ "setenta e oito"
             ]
  , examples (simple 80)
             [ "oitenta"
             ]
  , examples (simple 33)
             [ "33"
             , "trinta e três"
             , "trinta e tres"
             ]
  , examples (simple 1.1)
             [ "1,1"
             , "1,10"
             , "01,10"
             ]
  , examples (simple 0.77)
             [ "0,77"
             , ",77"
             , "ponto setenta e sete"
             ]
  , examples (simple 1000)
             [ "mil"
             ]
  , examples (simple 100000)
             [ "100.000"
             , "100000"
             , "100K"
             , "100k"
             , "100.000,00"
             ]
  , examples (simple 100)
             [ "100"
             , "Cem"
             ]
  , examples (simple 104)
             [ "104"
             , "cento e quatro"
             ]
  , examples (simple 243)
             [ "243"
             ,"duzentos e quarenta e tres"
             ]
  , examples (simple 300)
             [ "trezentos"
             ]
  , examples (simple 343)
             [ "trezentos e quarenta e tres"
             , "343"
             ]
  , examples (simple 891)
             [ "oitocentos e noventa e um"
             , "891"
             ]
  , examples (simple 2200)
             [ "dois mil e duzentos"
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
             , "menos 1.200.000"
             , "-1,2M"
             , "-1200K"
             , "-,0012G"
             , "negativo 1,2M"
             ]
  , examples (simple 1.5)
             [ "1 ponto cinco"
             , "um ponto cinco"
             , "1,5"
             ]
  , examples (simple 1200000.42)
             [ "1.200.000,42"
             ]
  , examples (simple 0.2)
             [ "1/5"
             , "2/10"
             , "3/15"
             , "20/100"
             ]
  ]
