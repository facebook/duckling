-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Number.PT.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Lang
import Duckling.Number.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {lang = PT}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumberValue 1)
             [ "1"
             , "um"
             , "Uma"
             ]
  , examples (NumberValue 2)
             [ "2"
             , "dois"
             , "duas"
             ]
  , examples (NumberValue 3)
             [ "3"
             , "três"
             , "tres"
             ]
  , examples (NumberValue 6)
             [ "6"
             , "seis"
             ]
  , examples (NumberValue 11)
             [ "11"
             , "onze"
             ]
  , examples (NumberValue 12)
             [ "12"
             , "doze"
             , "uma dúzia"
             , "uma duzia"
             ]
  , examples (NumberValue 14)
             [ "14"
             , "Catorze"
             , "quatorze"
             ]
  , examples (NumberValue 16)
             [ "16"
             , "dezesseis"
             , "dezasseis"
             ]
  , examples (NumberValue 17)
             [ "17"
             , "dezessete"
             , "dezassete"
             ]
  , examples (NumberValue 18)
             [ "18"
             , "dezoito"
             ]
  , examples (NumberValue 19)
             [ "19"
             , "dezenove"
             , "dezanove"
             ]
  , examples (NumberValue 21)
             [ "21"
             , "vinte e um"
             ]
  , examples (NumberValue 23)
             [ "23"
             , "vinte e tres"
             , "vinte e três"
             ]
  , examples (NumberValue 24)
             [ "24"
             , "vinte e quatro"
             , "duas dúzias"
             , "duas duzias"
             ]
  , examples (NumberValue 50)
             [ "50"
             , "cinquenta"
             , "cinqüenta"
             , "cincoenta"
             ]
  , examples (NumberValue 70)
             [ "setenta"
             ]
  , examples (NumberValue 78)
             [ "setenta e oito"
             ]
  , examples (NumberValue 80)
             [ "oitenta"
             ]
  , examples (NumberValue 33)
             [ "33"
             , "trinta e três"
             , "trinta e tres"
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
  , examples (NumberValue 100)
             [ "100"
             , "Cem"
             ]
  , examples (NumberValue 243)
             [ "243"
             ]
  , examples (NumberValue 300)
             [ "trezentos"
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
             , "menos 1.200.000"
             , "-1,2M"
             , "-1200K"
             , "-,0012G"
             ]
  , examples (NumberValue 1.5)
             [ "1 ponto cinco"
             , "um ponto cinco"
             , "1,5"
             ]
  ]
