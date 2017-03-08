-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.RU.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Lang
import Duckling.Ordinal.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {lang = RU}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (OrdinalData 1)
             [ "первый"
             , "первая"
             , "первое"
             , "1ая"
             , "1-ая"
             , "1ый"
             , "1-ый"
             , "1ое"
             , "1-ое"
             ]
  , examples (OrdinalData 4)
             [ "четвертый"
             , "четвертая"
             , "четвертое"
             , "4ый"
             , "4ая"
             , "4ое"
             , "4-ый"
             , "4-ая"
             , "4-ое"
             ]
  , examples (OrdinalData 15)
             [ "пятнадцатый"
             , "15й"
             , "15-й"
             ]
  , examples (OrdinalData 21)
             [ "21й"
             , "21-й"
             , "двадцать первый"
             ]
  , examples (OrdinalData 31)
             [ "31ый"
             , "31-ый"
             , "тридцать первый"
             ]
  , examples (OrdinalData 48)
             [ "48ое"
             , "48-ое"
             , "сорок восьмое"
             ]
  , examples (OrdinalData 99)
             [ "99ый"
             , "99-й"
             , "девяносто девятый"
             ]
  ]
