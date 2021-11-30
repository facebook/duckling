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
             , "первые"
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
             , "1-ого"
             , "первой"
             , "первым"
             , "первому"
             , "первым"
             ]
  , examples (OrdinalData 3)
             [ "третий"
             , "третья"
             , "третье"
             , "третьей"
             , "третьего"
             , "третьи"
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
             , "3-его"
             , "третьей"
             , "третьим"
             , "третьему"
             , "третьим"
             ]
  , examples (OrdinalData 4)
             [ "четвертый"
             , "четвертая"
             , "четвертое"
             , "четвертой"
             , "четвертого"
             , "четвёртый"
             , "четвертые"
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
             , "4-ого"
             , "четвертой"
             , "четвертым"
             , "четвёртому"
             , "четвёртым"
             ]
  , examples (OrdinalData 15)
             [ "пятнадцатый"
             , "15й"
             , "15-й"
             , "15-ый"
             , "пятнадцатая"
             , "15я"
             , "15-я"
             , "15-ая"
             , "пятнадцатое"
             , "15е"
             , "15-е"
             , "15-ое"
             , "пятнадцатому"
             , "пятнадцатые"
             , "пятнадцатой"
             , "пятнадцатым"
             , "пятнадцатому"
             , "пятнадцатым"
             ]
  , examples (OrdinalData 10)
             [ "десятый"
             , "10й"
             , "10-й"
             , "10-ый"
             , "десятая"
             , "десятой"
             ]
  , examples (OrdinalData 21)
             [ "21й"
             , "21-й"
             , "21-го"
             , "21-ого"
             , "Двадцать первый"
             , "двадцать первый"
             , "двадцать первого"
             , "двадцать первой"
             ]
  , examples (OrdinalData 23)
             [ "23й"
             , "23-й"
             , "двадцать третий"
             , "двадцать третьего"
             , "двадцать третьей"
             , "23-го"
             , "23-его"
             ]
  , examples (OrdinalData 31)
             [ "31ый"
             , "31-ый"
             , "тридцать первый"
             ]
  , examples (OrdinalData 30)
             [ "30ый"
             , "30-ый"
             , "тридцатый"
             ]
  , examples (OrdinalData 48)
             [ "48ое"
             , "48-ое"
             , "сорок восьмое"
             ]
  , examples (OrdinalData 40)
             [ "40ое"
             , "40-ое"
             , "сороковой"
             , "сороковое"
             , "сороковая"
             ]
  , examples (OrdinalData 99)
             [ "99ый"
             , "99-й"
             , "99-ый"
             , "девяносто девятый"
             , "девяносто девятая"
             ]
  , examples (OrdinalData 90)
             [ "90ый"
             , "90-й"
             , "90-ый"
             , "девяностый"
             , "девяностая"
             ]
  , examples (OrdinalData 100)
             [ "сотое"
             , "сотая"
             , "сотый"
             , "100-ая"
             , "100-я"
             , "100-й"
             , "100-е"
             ]
  ]
