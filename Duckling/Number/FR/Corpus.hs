-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Number.FR.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Lang
import Duckling.Number.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {lang = FR}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumberValue 0)
             [ "0"
             , "zero"
             , "zéro"
             ]
  , examples (NumberValue 1)
             [ "1"
             , "un"
             , "une"
             ]
  , examples (NumberValue 11)
             [ "onze"
             ]
  , examples (NumberValue 17)
             [ "dix sept"
             , "dix-sept"
             ]
  , examples (NumberValue 21)
             [ "vingt et un"
             , "vingt-et-un"
             ]
  , examples (NumberValue 23)
             [ "vingt trois"
             , "vingt-trois"
             ]
  , examples (NumberValue 70)
             [ "soixante dix"
             ]
  , examples (NumberValue 71)
             [ "soixante onze"
             ]
  , examples (NumberValue 78)
             [ "soixante dix huit"
             ]
  , examples (NumberValue 73)
             [ "soixante treize"
             ]
  , examples (NumberValue 80)
             [ "quatre vingt"
             ]
  , examples (NumberValue 81)
             [ "quatre vingt un"
             ]
  , examples (NumberValue 82)
             [ "quatre vingt deux"
             ]
  , examples (NumberValue 90)
             [ "quatre vingt dix"
             ]
  , examples (NumberValue 91)
             [ "quatre vingt onze"
             ]
  , examples (NumberValue 92)
             [ "quatre vingt douze"
             ]
  , examples (NumberValue 99)
             [ "quatre vingt dix neuf"
             ]
  , examples (NumberValue 33)
             [ "33"
             , "trente trois"
             , "trente-trois"
             , "trente 3"
             ]
  , examples (NumberValue 118)
             [ "cent dix-huit"
             ]
  , examples (NumberValue 4020)
             [ "quatre mille vingt"
             ]
  , examples (NumberValue 100000)
             [ "100.000"
             , "100000"
             , "100K"
             , "100k"
             , "cent mille"
             ]
  , examples (NumberValue 3000000)
             [ "3M"
             , "3000K"
             , "3000000"
             , "3.000.000"
             , "trois millions"
             ]
  , examples (NumberValue 1200000)
             [ "1.200.000"
             , "1200000"
             , "1,2M"
             , "1200K"
             , ",0012G"
             , "un million deux cent mille"
             ]
  , examples (NumberValue (-1200000))
             [ "- 1.200.000"
             , "-1200000"
             , "moins 1200000"
             , "-1,2M"
             , "-1200K"
             , "-,0012G"
             ]
  ]
