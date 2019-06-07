-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.UK.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Locale
import Duckling.Ordinal.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale UK Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (OrdinalData 1)
             [ "перший"
             , "перша"
             , "перше"
             , "1а"
             , "1-а"
             , "1ий"
             , "1-ий"
             , "1е"
             , "1-е"
             ]
  , examples (OrdinalData 4)
             [ "четвертий"
             , "четверта"
             , "четверте"
             , "4ий"
             , "4а"
             , "4е"
             , "4-ий"
             , "4-а"
             , "4-е"
             ]
  , examples (OrdinalData 15)
             [ "п‘ятнадцятий"
             , "15й"
             , "15-й"
             ]
  , examples (OrdinalData 21)
             [ "21й"
             , "21-й"
             , "двадцять перший"
             ]
  , examples (OrdinalData 31)
             [ "31ий"
             , "31-ий"
             , "тридцять перший"
             ]
  , examples (OrdinalData 48)
             [ "48е"
             , "48-е"
             , "сорок восьме"
             ]
  , examples (OrdinalData 99)
             [ "99ий"
             , "99-й"
             , "дев‘яносто дев‘ятий"
             ]
  ]
