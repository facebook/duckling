-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Number.ES.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Lang
import Duckling.Number.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {lang = ES}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumberValue 1)
             [ "1"
             , "uno"
             , "una"
             ]
  , examples (NumberValue 11)
             [ "once"
             ]
  , examples (NumberValue 16)
             [ "dieciséis"
             , "dieciseis"
             , "Diesiseis"
             , "diez y seis"
             ]
  , examples (NumberValue 21)
             [ "veintiuno"
             , "veinte y uno"
             ]
  , examples (NumberValue 23)
             [ "veintitrés"
             , "veinte y tres"
             ]
  , examples (NumberValue 70)
             [ "setenta"
             ]
  , examples (NumberValue 78)
             [ "Setenta y ocho"
             ]
  , examples (NumberValue 80)
             [ "ochenta"
             ]
  , examples (NumberValue 33)
             [ "33"
             , "treinta y tres"
             , "treinta y 3"
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
  , examples (NumberValue 300)
             [ "trescientos"
             ]
  , examples (NumberValue 243)
             [ "243"
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
             [ "1 punto cinco"
             , "una punto cinco"
             , "1,5"
             ]
  ]
