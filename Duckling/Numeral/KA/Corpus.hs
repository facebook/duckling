-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.KA.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

context :: Context
context = testContext {locale = makeLocale KA Nothing}

corpus :: Corpus
corpus = (context, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple 0)
             [ "0"
             , "ნოლი"
             , "ნული"
             , "ნოლ"
             ]
  , examples (simple 1)
             [ "1"
             , "ერთი"
             ]
  , examples (simple 2)
             [ "2"
             , "ორი"
             , "წყვილი"
             , "წყვილები"
             ]
  , examples (simple 3)
             [ "3"
             , "სამი"
             , "ცოტა"
             , "რამდენიმე"
             , "რამოდენიმე"
             ]
  , examples (simple 4)
             [ "4"
             , "ოთხი"
             ]
  , examples (simple 5)
             [ "5"
             , "ხუთი"
             ]
  , examples (simple 6)
             [ "6"
             , "ექვსი"
             ]
  , examples (simple 7)
             [ "7"
             , "შვიდი"
             ]
  , examples (simple 8)
             [ "8"
             , "რვა"
             ]
  , examples (simple 9)
             [ "9"
             , "ცხრა"
             ]
  , examples (simple 10)
             [ "10"
             , "ათი"
             ]
  , examples (simple 11)
             [ "11"
             , "თერთმეტი"
             ]
  , examples (simple 12)
             [ "12"
             , "თორმეტი"
             ]
  , examples (simple 13)
             [ "13"
             , "ცამეტი"
             ]
  , examples (simple 14)
             [ "14"
             , "თოთხმეტი"
             ]
  , examples (simple 15)
             [ "15"
             , "თხუთმეტი"
             ]
  , examples (simple 16)
             [ "16"
             , "თექვსმეტი"
             ]
  , examples (simple 17)
             [ "17"
             , "ჩვიდმეტი"
             ]
  , examples (simple 18)
             [ "18"
             , "თვრამეტი"
             ]
  , examples (simple 19)
             [ "19"
             , "ცხრამეტი"
             ]
  , examples (simple 20)
             [ "20"
             , "ოცი"
             ]
  , examples (simple 21)
             [ "21"
             , "ოცდაერთი"
             ]
  , examples (simple 22)
             [ "22"
             , "ოცდაორი"
             ]
  , examples (simple 23)
             [ "23"
             , "ოცდასამი"
             ]
  , examples (simple 24)
             [ "24"
             , "ოცდაოთხი"
             ]
  , examples (simple 25)
             [ "25"
             , "ოცდახუთი"
             ]
  , examples (simple 26)
             [ "26"
             , "ოცდაექვსი"
             ]
  , examples (simple 27)
             [ "27"
             , "ოცდაშვიდი"
             ]
  , examples (simple 30)
             [ "30"
             , "ოცდაათი"
             ]
  , examples (simple 31)
             [ "31"
             , "ოცდათერთმეტი"
             ]
  , examples (simple 38)
             [ "38"
             , "ოცდათვრამეტი"
             ]
  , examples (simple 39)
             [ "39"
             , "ოცდაცხრამეტი"
             ]
  , examples (simple 40)
             [ "40"
             , "ორმოცი"
             ]
  , examples (simple 42)
             [ "42"
             , "ორმოცდაორი"
             ]
  , examples (simple 47)
             [ "47"
             , "ორმოცდაშვიდი"
             ]
  , examples (simple 99)
             [ "99"
             , "ოთხმოცდაცხრამეტი"
             ]
  , examples (simple 100)
             [ "100"
             , "ასი"
             ]
  , examples (simple 101)
             [ "101"
             , "ას ერთი"
             ]
  , examples (simple 102)
             [ "102"
             , "ას ორი"
             ]
  , examples (simple 121)
             [ "121"
             , "ას ოცდაერთი"
             ]
  , examples (simple 200)
             [ "200"
             , "ორასი"
             ]
  , examples (simple 201)
             [ "201"
             , "ორას ერთი"
             ]
  , examples (simple 250)
             [ "250"
             , "ორას ორმოცდაათი"
             ]
  , examples (simple 300)
             [ "300"
             , "სამასი"
             ]
  , examples (simple 310)
             [ "310"
             , "სამას ათი"
             ]
  , examples (simple 415)
             [ "415"
             , "ოთხას თხუთმეტი"
             ]
  , examples (simple 600)
             [ "600"
             , "ექვსასი"
             ]
  , examples (simple 909)
             [ "909"
             , "ცხრაას ცხრა"
             ]
  , examples (simple 999)
             [ "999"
             , "ცხრაას ოთხმოცდაცხრამეტი"
             ]
  , examples (simple 1000)
             [ "1000"
             , "ათასი"
             ]
  , examples (simple 1001)
             [ "1001"
             , "ათას ერთი"
             ]
  , examples (simple 2000)
             [ "2000"
             , "ორი ათასი"
             ]
  , examples (simple 2010)
             [ "2010"
             , "ორი ათას ათი"
             ]
  , examples (simple 7892)
             [ "7892"
             , "შვიდი ათას რვაას ოთხმოცდათორმეტი"
             ]
  , examples (simple 10000)
             [ "10000"
             , "ათი ათასი"
             ]
  , examples (simple 121000)
             [ "121000"
             , "ას ოცდაერთი ათასი"
             ]
  , examples (simple 120000)
             [ "120000"
             , "ას ოცი ათასი"
             ]
  , examples (simple 124000)
             [ "124000"
             , "ას ოცდაოთხი ათასი"
             ]
  , examples (simple 999000)
             [ "999000"
             , "ცხრაას ოთხმოცდაცხრამეტი ათასი"
             ]
  , examples (simple 999900)
             [ "999900"
             , "ცხრაას ოთხმოცდაცხრამეტი ათას ცხრაასი"
             ]
  , examples (simple 999990)
             [ "999990"
             , "ცხრაას ოთხმოცდაცხრამეტი ათას ცხრაას ოთხმოცდაათი"
             ]
  , examples (simple 999999)
             [ "999999"
             , "ცხრაას ოთხმოცდაცხრამეტი ათას ცხრაას ოთხმოცდაცხრამეტი"
             ]
  , examples (simple 1000000)
             [ "1000000"
             , "ერთი მილიონი"
             ]
  , examples (simple 2000000)
             [ "2000000"
             , "ორი მილიონი"
             ]
  , examples (simple 999000999)
             [ "999000999"
             , "ცხრაას ოთხმოცდაცხრამეტი მილიონ ცხრაას ოთხმოცდაცხრამეტი"
             ]
  , examples (simple 999999999)
             [ "999999999"
             , "ცხრაას ოთხმოცდაცხრამეტი მილიონ ცხრაას ოთხმოცდაცხრამეტი ათას ცხრაას ოთხმოცდაცხრამეტი"
             ]
  , examples (simple 1174315110)
             [ "1174315110"
             , "ერთი მილიარდ ას სამოცდათოთხმეტი მილიონ სამას თხუთმეტი ათას ას ათი"
             ]
  , examples (simple 1174315119)
             [ "1174315119"
             , "ერთი მილიარდ ას სამოცდათოთხმეტი მილიონ სამას თხუთმეტი ათას ას ცხრამეტი"
             ]
  , examples (simple 15174315110)
             [ "15174315110"
             , "თხუთმეტი მილიარდ ას სამოცდათოთხმეტი მილიონ სამას თხუთმეტი ათას ას ათი"
             ]
  , examples (simple 35174315119)
             [ "35174315119"
             , "ოცდათხუთმეტი მილიარდ ას სამოცდათოთხმეტი მილიონ სამას თხუთმეტი ათას ას ცხრამეტი"
             ]
  , examples (simple 935174315119)
             [ "935174315119"
             , "ცხრაას ოცდათხუთმეტი მილიარდ ას სამოცდათოთხმეტი მილიონ სამას თხუთმეტი ათას ას ცხრამეტი"
             ]
  , examples (simple 33)
             [ "33"
             , "ოცდაცამეტი"
             , "0033"
             ]
  , examples (simple 24)
             [ "24"
             , "ოცდაოთხი"
             ]
  , examples (simple 1.1)
             [ "1.1"
             , "1.10"
             , "01.10"
             , "1 მთელი 1"
             , "ერთი მთელი ერთი"
             ]
  , examples (simple 0.77)
             [ ".77"
             , "0.77"
             , "ნოლი მთელი სამოცდაჩვიდმეტი"
             , "ნული მთელი სამოცდაჩვიდმეტი"
             ]
  , examples (simple 100000)
             [ "100,000"
             , "100,000.0"
             , "100000"
             , "100k"
             , "ასი ათასი"
             ]
  , examples (simple 0.2)
             [ "1/5"
             , "2/10"
             , "3/15"
             , "20/100"
             ]
  , examples (simple 3e6)
             [ "3M"
             , "3000K"
             , "3000000"
             , "3,000,000"
             , "3 მილიონი"
             , "სამი მილიონი"
             ]
  , examples (simple 1.2e6)
             [ "1,200,000"
             , "1200000"
             , "1.2M"
             , "1200k"
             , ".0012G"
             ]
  , examples (simple 5000)
             [ "5 ათასი"
             , "ხუთი ათასი"
             ]
  , examples (simple (-504))
             [ "-504"
             , "მინუს ხუთას ოთხი"
             , "მინუს 504"
             ]
  , examples (simple (-1.2e6))
             [ "- 1,200,000"
             , "-1200000"
             , "მინუს 1,200,000"
             , "მინუს 1200000"
             , "-1.2M"
             , "-1200K"
             , "-.0012G"
             ]
  , examples (simple 122)
             [ "ას ოცდაორი"
             , "ასოცდაორი"
             ]
  , examples (simple 2e5)
             [ "ორასი ათასი"
             ]
  , examples (simple 21011)
             [ "ოცდაერთიათას თერთმეტი"
             ]
  , examples (simple 721012)
             [ "შვიდას ოცდაერთი ათას თორმეტი"
             ]
  , examples (simple 31256721)
             [ "ოცდათერთმეტი მილიონ ორას ორმოცდათექვსმეტი ათას შვიდას ოცდაერთი"
             ]
  , examples (simple 2400)
             [ "ორიათას ოთხასი"
             , "ორიათასოთხასი"
             ]
  , examples (simple 2200000)
             [ "ორი მთელი ორი მილიონი"
             , "ორი მილიონ ორასი ათას"
             ]
  , examples (simple 3000000000)
             [ "სამი მილიარდი"
             , "3 მილიარდი"
             ]
  ]
