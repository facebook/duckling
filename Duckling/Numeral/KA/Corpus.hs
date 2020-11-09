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
  [ examples (NumeralValue 0)
             [ "0"
             , "ნოლი"
             , "ნული"
             , "ნოლ"
             ]
  , examples (NumeralValue 1)
             [ "1"
             , "ერთი"
             ]
  , examples (NumeralValue 2)
             [ "2"
             , "ორი"
             , "წყვილი"
             , "წყვილები"
             ]
  , examples (NumeralValue 3)
             [ "3"
             , "სამი"
             , "ცოტა"
             , "რამდენიმე"
             , "რამოდენიმე"
             ]
  , examples (NumeralValue 4)
             [ "4"
             , "ოთხი"
             ]
  , examples (NumeralValue 5)
             [ "5"
             , "ხუთი"
             ]
  , examples (NumeralValue 6)
             [ "6"
             , "ექვსი"
             ]
  , examples (NumeralValue 7)
             [ "7"
             , "შვიდი"
             ]
  , examples (NumeralValue 8)
             [ "8"
             , "რვა"
             ]
  , examples (NumeralValue 9)
             [ "9"
             , "ცხრა"
             ]
  , examples (NumeralValue 10)
             [ "10"
             , "ათი"
             ]
  , examples (NumeralValue 11)
             [ "11"
             , "თერთმეტი"
             ]
  , examples (NumeralValue 12)
             [ "12"
             , "თორმეტი"
             ]
  , examples (NumeralValue 13)
             [ "13"
             , "ცამეტი"
             ]
  , examples (NumeralValue 14)
             [ "14"
             , "თოთხმეტი"
             ]
  , examples (NumeralValue 15)
             [ "15"
             , "თხუთმეტი"
             ]
  , examples (NumeralValue 16)
             [ "16"
             , "თექვსმეტი"
             ]
  , examples (NumeralValue 17)
             [ "17"
             , "ჩვიდმეტი"
             ]
  , examples (NumeralValue 18)
             [ "18"
             , "თვრამეტი"
             ]
  , examples (NumeralValue 19)
             [ "19"
             , "ცხრამეტი"
             ]
  , examples (NumeralValue 20)
             [ "20"
             , "ოცი"
             ]
  , examples (NumeralValue 21)
             [ "21"
             , "ოცდაერთი"
             ]
  , examples (NumeralValue 22)
             [ "22"
             , "ოცდაორი"
             ]
  , examples (NumeralValue 23)
             [ "23"
             , "ოცდასამი"
             ]
  , examples (NumeralValue 24)
             [ "24"
             , "ოცდაოთხი"
             ]
  , examples (NumeralValue 25)
             [ "25"
             , "ოცდახუთი"
             ]
  , examples (NumeralValue 26)
             [ "26"
             , "ოცდაექვსი"
             ]
  , examples (NumeralValue 27)
             [ "27"
             , "ოცდაშვიდი"
             ]
  , examples (NumeralValue 30)
             [ "30"
             , "ოცდაათი"
             ]
  , examples (NumeralValue 31)
             [ "31"
             , "ოცდათერთმეტი"
             ]
  , examples (NumeralValue 38)
             [ "38"
             , "ოცდათვრამეტი"
             ]
  , examples (NumeralValue 39)
             [ "39"
             , "ოცდაცხრამეტი"
             ]
  , examples (NumeralValue 40)
             [ "40"
             , "ორმოცი"
             ]
  , examples (NumeralValue 42)
             [ "42"
             , "ორმოცდაორი"
             ]
  , examples (NumeralValue 47)
             [ "47"
             , "ორმოცდაშვიდი"
             ]
  , examples (NumeralValue 99)
             [ "99"
             , "ოთხმოცდაცხრამეტი"
             ]
  , examples (NumeralValue 100)
             [ "100"
             , "ასი"
             ]
  , examples (NumeralValue 101)
             [ "101"
             , "ას ერთი"
             ]
  , examples (NumeralValue 102)
             [ "102"
             , "ას ორი"
             ]
  , examples (NumeralValue 121)
             [ "121"
             , "ას ოცდაერთი"
             ]
  , examples (NumeralValue 200)
             [ "200"
             , "ორასი"
             ]
  , examples (NumeralValue 201)
             [ "201"
             , "ორას ერთი"
             ]
  , examples (NumeralValue 250)
             [ "250"
             , "ორას ორმოცდაათი"
             ]
  , examples (NumeralValue 300)
             [ "300"
             , "სამასი"
             ]
  , examples (NumeralValue 310)
             [ "310"
             , "სამას ათი"
             ]
  , examples (NumeralValue 415)
             [ "415"
             , "ოთხას თხუთმეტი"
             ]
  , examples (NumeralValue 600)
             [ "600"
             , "ექვსასი"
             ]
  , examples (NumeralValue 909)
             [ "909"
             , "ცხრაას ცხრა"
             ]
  , examples (NumeralValue 999)
             [ "999"
             , "ცხრაას ოთხმოცდაცხრამეტი"
             ]
  , examples (NumeralValue 1000)
             [ "1000"
             , "ათასი"
             ]
  , examples (NumeralValue 1001)
             [ "1001"
             , "ათას ერთი"
             ]
  , examples (NumeralValue 2000)
             [ "2000"
             , "ორი ათასი"
             ]
  , examples (NumeralValue 2010)
             [ "2010"
             , "ორი ათას ათი"
             ]
  , examples (NumeralValue 7892)
             [ "7892"
             , "შვიდი ათას რვაას ოთხმოცდათორმეტი"
             ]
  , examples (NumeralValue 10000)
             [ "10000"
             , "ათი ათასი"
             ]
  , examples (NumeralValue 121000)
             [ "121000"
             , "ას ოცდაერთი ათასი"
             ]
  , examples (NumeralValue 120000)
             [ "120000"
             , "ას ოცი ათასი"
             ]
  , examples (NumeralValue 124000)
             [ "124000"
             , "ას ოცდაოთხი ათასი"
             ]
  , examples (NumeralValue 999000)
             [ "999000"
             , "ცხრაას ოთხმოცდაცხრამეტი ათასი"
             ]
  , examples (NumeralValue 999900)
             [ "999900"
             , "ცხრაას ოთხმოცდაცხრამეტი ათას ცხრაასი"
             ]
  , examples (NumeralValue 999990)
             [ "999990"
             , "ცხრაას ოთხმოცდაცხრამეტი ათას ცხრაას ოთხმოცდაათი"
             ]
  , examples (NumeralValue 999999)
             [ "999999"
             , "ცხრაას ოთხმოცდაცხრამეტი ათას ცხრაას ოთხმოცდაცხრამეტი"
             ]
  , examples (NumeralValue 1000000)
             [ "1000000"
             , "ერთი მილიონი"
             ]
  , examples (NumeralValue 2000000)
             [ "2000000"
             , "ორი მილიონი"
             ]
  , examples (NumeralValue 999000999)
             [ "999000999"
             , "ცხრაას ოთხმოცდაცხრამეტი მილიონ ცხრაას ოთხმოცდაცხრამეტი"
             ]
  , examples (NumeralValue 999999999)
             [ "999999999"
             , "ცხრაას ოთხმოცდაცხრამეტი მილიონ ცხრაას ოთხმოცდაცხრამეტი ათას ცხრაას ოთხმოცდაცხრამეტი"
             ]
  , examples (NumeralValue 1174315110)
             [ "1174315110"
             , "ერთი მილიარდ ას სამოცდათოთხმეტი მილიონ სამას თხუთმეტი ათას ას ათი"
             ]
  , examples (NumeralValue 1174315119)
             [ "1174315119"
             , "ერთი მილიარდ ას სამოცდათოთხმეტი მილიონ სამას თხუთმეტი ათას ას ცხრამეტი"
             ]
  , examples (NumeralValue 15174315110)
             [ "15174315110"
             , "თხუთმეტი მილიარდ ას სამოცდათოთხმეტი მილიონ სამას თხუთმეტი ათას ას ათი"
             ]
  , examples (NumeralValue 35174315119)
             [ "35174315119"
             , "ოცდათხუთმეტი მილიარდ ას სამოცდათოთხმეტი მილიონ სამას თხუთმეტი ათას ას ცხრამეტი"
             ]
  , examples (NumeralValue 935174315119)
             [ "935174315119"
             , "ცხრაას ოცდათხუთმეტი მილიარდ ას სამოცდათოთხმეტი მილიონ სამას თხუთმეტი ათას ას ცხრამეტი"
             ]
  , examples (NumeralValue 33)
             [ "33"
             , "ოცდაცამეტი"
             , "0033"
             ]
  , examples (NumeralValue 24)
             [ "24"
             , "ოცდაოთხი"
             ]
  , examples (NumeralValue 1.1)
             [ "1.1"
             , "1.10"
             , "01.10"
             , "1 მთელი 1"
             , "ერთი მთელი ერთი"
             ]
  , examples (NumeralValue 0.77)
             [ ".77"
             , "0.77"
             , "ნოლი მთელი სამოცდაჩვიდმეტი"
             , "ნული მთელი სამოცდაჩვიდმეტი"
             ]
  , examples (NumeralValue 100000)
             [ "100,000"
             , "100,000.0"
             , "100000"
             , "100k"
             , "ასი ათასი"
             ]
  , examples (NumeralValue 0.2)
             [ "1/5"
             , "2/10"
             , "3/15"
             , "20/100"
             ]
  , examples (NumeralValue 3e6)
             [ "3M"
             , "3000K"
             , "3000000"
             , "3,000,000"
             , "3 მილიონი"
             , "სამი მილიონი"
             ]
  , examples (NumeralValue 1.2e6)
             [ "1,200,000"
             , "1200000"
             , "1.2M"
             , "1200k"
             , ".0012G"
             ]
  , examples (NumeralValue 5000)
             [ "5 ათასი"
             , "ხუთი ათასი"
             ]
  , examples (NumeralValue (-504))
             [ "-504"
             , "მინუს ხუთას ოთხი"
             , "მინუს 504"
             ]
  , examples (NumeralValue (-1.2e6))
             [ "- 1,200,000"
             , "-1200000"
             , "მინუს 1,200,000"
             , "მინუს 1200000"
             , "-1.2M"
             , "-1200K"
             , "-.0012G"
             ]
  , examples (NumeralValue 122)
             [ "ას ოცდაორი"
             , "ასოცდაორი"
             ]
  , examples (NumeralValue 2e5)
             [ "ორასი ათასი"
             ]
  , examples (NumeralValue 21011)
             [ "ოცდაერთიათას თერთმეტი"
             ]
  , examples (NumeralValue 721012)
             [ "შვიდას ოცდაერთი ათას თორმეტი"
             ]
  , examples (NumeralValue 31256721)
             [ "ოცდათერთმეტი მილიონ ორას ორმოცდათექვსმეტი ათას შვიდას ოცდაერთი"
             ]
  , examples (NumeralValue 2400)
             [ "ორიათას ოთხასი"
             , "ორიათასოთხასი"
             ]
  , examples (NumeralValue 2200000)
             [ "ორი მთელი ორი მილიონი"
             , "ორი მილიონ ორასი ათას"
             ]
  , examples (NumeralValue 3000000000)
             [ "სამი მილიარდი"
             , "3 მილიარდი"
             ]
  ]
