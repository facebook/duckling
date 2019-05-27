-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.AR.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.AmountOfMoney.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale AR Nothing},
  testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Dollar 1)
             [ "$1"
             , "1 دولار"
             ]
  , examples (simple Dollar 10)
             [ "$10"
             , "$ 10"
             , "10 $"
             , "10$"
             , "10$"
             , "10 دولار"
             , "عشرة دولارات"
             ]
  , examples (simple Cent 10)
             [ "10 سنتات"
             , "10 سنت"
             , "10 سينت"
             , "10 سينتات"
             , "10 c"
             , "10¢"
             ]
  , examples (simple Dollar 1e4)
             [ "10000 دولار"
             , "$10,000"
             , "10000$"
             ]
  , examples (simple EUR 20)
             [ "20 يورو"
             , "20 اورو"
             , "20 أورو"
             , "20€"
             , "20 €"
             ]
  , examples (simple Pound 10)
             [ "10 جنيه"
             , "10 جنيهات"
             ]
  , examples (simple Dollar 20.43)
             [ "20$ و 43c"
             , "$20 43"
             , "20$ 43c"
             , "20 دولار و43 سنت"
             , "20.43 $"
             ]
  , examples (simple GBP 3)
             [ "3 جنيهات استرلينية"
             , "3 جنيه استرليني"
             ]
  , examples (simple KWD 42)
             [ "42 KWD"
             , "42 دينار كويتي"
             ]
  , examples (simple LBP 42)
             [ "42 LBP"
             , "42 ليرة لبنانية"
             , "42 ليرات لبنانية"
             ]
  , examples (simple EGP 42)
             [ "42 EGP"
             , "42 جنيه مصري"
             , "42 جنيهات مصريه"
             ]
  , examples (simple QAR 42)
             [ "42 QAR"
             , "42 ريال قطري"
             , "42 ريالات قطرية"
             ]
  , examples (simple SAR 42)
             [ "42 SAR"
             , "42 ريال سعودي"
             ]
  , examples (between Dollar (10, 20))
             [ "من 10 الى 20 دولار"
             , "بحدود 10-20 دولار"
             , "ما بين عشرة وعشرون دولار"
             , "من عشرة لعشرين دولار"
             , "حوالي 10$-20$"
             , "10-20 $"
             , "10-20 دولار"
             ]
  , examples (under EUR 7)
             [ "تحت سبعة اورو"
             , "اقل من سبعة يورو"
             , "اقل من 7 يورو"
             ]
  , examples (above Dollar 3.42)
             [ "اكثر من ثلاثة دولار و42 سينت"
             , "3.42$ على الاقل"
             ]
  , examples (simple JOD 5)
             [ "5 دينار اردني"
             , "5 دنانير أردنية"
             ]
  , examples (simple ILS 5)
             [ "5 شيقل"
             , "5 شواقل"
             , "خمسة شيكل"
             ]
  ]
