-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.RU.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Locale
import Duckling.Ordinal.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale RU Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (OrdinalData 1)
             [ "первый"
             , "Первая"
             , "первая"
             , "первое"
             , "первой"
             , "первого"
             , "1ая"
             , "1-ая"
             , "1ый"
             , "1-ый"
             , "1ое"
             , "1-ое"
             , "1й"
             , "1-й"
             , "1го"
             , "1-го"
             ]
  , examples (OrdinalData 3)
             [ "третий"
             , "третья"
             , "третье"
             , "третьей"
             , "третьего"
             , "3й"
             , "3ий"
             , "3я"
             , "3ья"
             , "3е"
             , "3ье"
             , "3го"
             , "3-й"
             , "3-ий"
             , "3-я"
             , "3-ья"
             , "3-е"
             , "3-ье"
             , "3-го"
             ]
  , examples (OrdinalData 4)
             [ "четвертый"
             , "четвертая"
             , "четвертое"
             , "четвертой"
             , "четвертого"
             , "4й"
             , "4ый"
             , "4ая"
             , "4ое"
             , "4ой"
             , "4го"
             , "4-й"
             , "4-ый"
             , "4-ая"
             , "4-ое"
             , "4-ой"
             , "4-го"
             ]
  , examples (OrdinalData 15)
             [ "пятнадцатый"
             , "15й"
             , "15-й"
             ]
  , examples (OrdinalData 21)
             [ "21й"
             , "21-й"
             , "21-го"
             , "Двадцать первый"
             , "двадцать первый"
             , "двадцать первого"
             ]
  , examples (OrdinalData 23)
             [ "23й"
             , "23-й"
             , "двадцать третий"
             , "двадцать третьего"
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
