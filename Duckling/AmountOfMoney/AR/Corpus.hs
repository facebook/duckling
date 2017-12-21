-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.EN.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.AmountOfMoney.Types
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Dollar 1)
             [ "$1"
             , "دولارامريكي واحد"
             , "1 دولار"
             ]
  , examples (simple Dollar 10)
             [ "$10"
             , "$ 10"
			 , "10 $"
			 , "10$"
             , "10$"
             , "10 دولار"
             , "عشرة دولارات أمريكية"
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
             ]
  , examples (simple USD 3.14)
             [ "3.14 دولار"
             , "3 دولارات و14 سنت"
             , "3 دولار و 14 سينت"
			 , "3.14 دولارات"
             ]
  , examples (simple EUR 20)
             [ "20 يورو"
             , "20 اورو"
             , "20 أورو"
             , "20 ايرو"
			 , "20 أيرو"
             , "20€"
             , "20 €"
             ]
  , examples (simple Pound 10)
             [ "10 جنيه"
             , "10 جنيهات"
			 , "10 ليرة"
			 , "10 ليرات"
             ]
  , examples (simple INR 20)
             [ "20 روبية هندية"
             , "20 روبيات هندية"
             ]
  , examples (simple INR 20.43)
             [ "20.43 روبية هندية"
             , "20 روبية هندية و 43 بيزة"
             ]
  , examples (simple Dollar 20.43)
             [ "20$ و 43c"
             , "$20 43"
             , "20$ 43c"
             , "20 دولار و43 سنت"
             , "20.43 $"
             ]
  , examples (simple GBP 3.01)
             [ "GBP3.01"
             , "GBP 3.01"
             , "3 جنيهات استرلينية"
			 , "3 جنيه استرليني"
             ]
  , examples (simple Unnamed 42)
             [ "42 قطعة"
             , "42 قطعة نقدية"
             , "42"
             ]
  , examples (simple KWD 42)
             [ "42 KWD"
             , "42 دينار كويتي"
			 , "42 من الدينارات الكويتية"
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
  , examples (simple BGN 42)
             [ "42 BGN"
             , "42 ليف بلغاري"
             , "42 ليفات بلغارية"
             ]
  , examples (simple MYR 42)
             [ "42 MYR"
             , "42 RM"
             , "RM 42"
             , "MYR 42"
             , "42MYR"
             , "42RM"
             , "RM42"
             , "MYR42"
             , "42 رينغيت ماليزي"
             , "42 رينغت ماليزي"
             , "42 رينغيت"
             , "42 رينغت"
             ]
  , examples (simple MYR 20.43)
             [ "20 ringgit and 43c"
             , "20 ringgit and 43 sen"
             , "twenty ringgit 43 sens"
             , "20 ringgit 43"
             , "twenty ringgit and 43"
             ]
  , examples (between Dollar (10, 20))
             [ "من 10 الى 20 دولار"
             , "بين 10 و20 دولار امريكي"
             , "بحدود 10-20 دولار"
             , "ما بين العشرة والعشرون دولار أمريكي"
             , "من عشرة لعشرين دولار"
             , "حوالي 10$-20$"
             , "10-20 $"
             , "10-20 دولار"
             ]
  , examples (under EUR 7)
             [ "تحت السبعة اورو"
             , "اقل من 7 يورو"
             , "اقل من 7 ايرو"
             ]
  , examples (above Dollar 1.42)
             [ "اكثر من دولار و42 سينت"
             , "1.42$ على الاقل"
             , "فوق ال1.42 دولار"
             , "فوق الدولار والاثنان واربعون سنتا امريكيا"
             ]





  , examples (simple JOD 5)
             [ "5 دينار اردني"
             , "5 دنانير أردنية"
             , "5 ليرات اردنية"
             ]
  , examples (simple JOD 5.67)
             [ "5.67 دينار اردني"
             , "5 دنانير و67 قرش اردني"
             , "5 ليرات اردنية و 67 وقرش"
             ]
  , examples (simple NIS 5)
             [ "5 شيقل"
             , "5 شواقل اسرائيلية"
             , "خمسة شيقل اسرائيلي"
             ]
  , examples (simple NIS 5.5)
             [ "5.5 شيقل"
             , "5 شيقل و50 اغورة"
             , "5 شيقل ونص"
             , "5 شيقل و نصف"
             ]
  ]
