-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.RU.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.AmountOfMoney.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale RU Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple RUB 1)
             [ "1 rub"
             , "один рубль"
             , "1 ₽"
             , "1 рубль"
             ]
  , examples (simple RUB 10)
             [ "10 рублей"
             , "₽ 10"
             , "10₽"
             , "10RUB"
             , "10руб"
             , "10 рублей"
             , "рублей 10"
             , "10 рублях"
             ]
  , examples (simple Dollar 1)
             [ "$1"
             , "один доллар"
             ]
  , examples (simple Dollar 10)
             [ "$10"
             , "$ 10"
             , "10$"
             , "10 долларов"
             , "десять долларов"
             ]
  , examples (simple Cent 10)
             [ "10 центов"
             , "десять пени"
             , "десять центов"
             , "10 c"
             , "10¢"
             ]
  , examples (simple Dollar 1e4)
             [ "$10К"
             , "10к$"
             , "$10,000"
             ]
  , examples (simple USD 3.14)
             [ "USD3.14"
             , "3.14US$"
             , "US$ 3.14"
             ]
  , examples (simple EUR 20)
             [ "20\x20ac"
             , "20 euros"
             , "20 Euro"
             , "20 Euros"
             , "EUR 20"
             , "EUR 20.0"
             , "20€"
             , "20 €ur"
             , "20 евро"
             , "Евро 20"
             ]
  , examples (simple Pound 10)
             [ "\x00a3\&10"
             , "десять фунтов"
             ]
  , examples (simple INR 20)
             [ "Rs. 20"
             , "Rs 20"
             , "20Rs"
             , "Rs20"
             ]
  , examples (simple Dollar 20.43)
             [ "$20 и 43ц"
             , "$20 43"
             , "20 долларов 43ц"
             , "20 долларов 43 центов"
             , "20 долларами 43 центами"
             , "20 долларов 43"
             , "двадцать долларов и 43"
             ]
  , examples (simple GBP 3.01)
             [ "GBP3.01"
             , "GBP 3.01"
             , "3 GBP 1 пенс"
             ]
  , examples (simple Unnamed 42)
             [ "42 бакса"
             , "бакса 42"
             , "42 баксов"
             ]
  , examples (simple BYN 42)
             [ "42 BYN"
             ]
  , examples (simple KWD 42)
             [ "42 KWD"
             ]
  , examples (simple LBP 42)
             [ "42 LBP"
             ]
  , examples (simple EGP 42)
             [ "42 EGP"
             ]
  , examples (simple QAR 42)
             [ "42 QAR"
             ]
  , examples (simple SAR 42)
             [ "42 SAR"
             ]
  , examples (simple BGN 42)
             [ "42 BGN"
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
             ]
  , examples (between Dollar (10, 20))
             [ "между 10 и 20 долларами"
             , "от 10 долларов до 20"
             , "10-20 долларов"
             , "между 10 долларами и 20 долларами"
             , "от 10 до 20 долларов"
             , "10$-20$"
             , "10-20 долларов"
             ]
  , examples (under EUR 7)
             [ "менее 7 евро"
             , "меньше чем 7 EUR"
             , "ниже 7€"
             , "меньше 7 евро"
             , "не больше 7 евро"
             , "не более 7 евро"
             ]
  , examples (above Dollar 1.42)
             [ "больше чем 1 доллар и сорок два цента"
             , "как минимум $1.42"
             , "более 1.42 долларов"
             , "выше 1 доллара и 42 центов"
             , "свыше 1 доллара и 42 центов"
             ]
  ]
