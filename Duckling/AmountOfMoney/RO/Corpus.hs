-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.RO.Corpus
  ( corpus
  , negativeCorpus
  ) where

import Data.String
import Prelude

import Duckling.AmountOfMoney.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types

context :: Context
context = testContext {locale = makeLocale RO Nothing}

negativeCorpus :: NegativeCorpus
negativeCorpus = (context, testOptions, examples)
  where
    examples =
      [ "10 de dolari"
      ]

corpus :: Corpus
corpus = (context, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple RON 10)
             [ "10 lei"
             , "10 roni"
             , "10 RON"
             ]
  , examples (simple Cent 50)
             [ "50 de bani"
             , "50 DE BANI"
             ]
  , examples (simple RON 10.5)
             [ "10,5 lei"
             , "10,5 ron"
             , "10 lei si 50 bani"
             ]
  , examples (simple Dollar 10)
             [ "$10"
             , "10$"
             , "zece dolari"
             , "10 dolari"
             ]
  , examples (simple Cent 10)
             [ "zece centi"
             , "zece cenți"
             ]
  , examples (simple Dollar 10000)
             [ "$10.000"
             , "$10000"
             ]
  , examples (simple USD 1.23)
             [ "USD1,23"
             ]
  , examples (simple Dollar 1)
             [ "1 dolar"
             , "un dolar"
             , "$1"
             ]
  , examples (simple Cent 2)
             [ "2 centi"
             ]
  , examples (simple Cent 23)
             [ "23 de centi"
             ]
  , examples (simple Dollar 2.23)
             [ "2 dolari si 23 de centi"
             , "2 dolari și 23 de cenți"
             , "doi dolari si douăzeci si trei de centi"
             , "doi dolari și douăzeci și trei de cenți"
             , "2 dolari 23 de centi"
             , "doi dolari si 23"
             , "doi dolari și 23"
             ]
  , examples (simple EUR 20)
             [ "20€"
             , "20 de euro"
             , "20 de Euro"
             , "EUR 20"
             ]
  , examples (simple EUR 29.99)
             [ "EUR29,99"
             ]
  , examples (simple Pound 9)
             [ "£9"
             , "noua lir"
             , "nouă lire"
             ]
  , examples (simple GBP 3.01)
             [ "GBP3,01"
             , "GBP 3,01"
             ]
  , examples (simple QAR 1)
             [ "un rial qatarian"
             , "1 rial qataria"
             ]
  , examples (simple EGP 10)
             [ "zece lira egiptiana"
             ]
  , examples (simple LBP 1)
             [ "una liră libaneză"
             ]
  , examples (simple INR 42)
             [ "42 de rupii"
             , "Rs. 42"
             ]
  , examples (simple KWD 1)
             [ "un dinar kuweitian"
             ]
  , examples (simple AED 2)
             [ "2 dirhami"
             ]
  , examples (simple SAR 1)
             [ "1 rial saudit"
             , "un rial saudi"
             ]
  , examples (between RON (5, 15))
             [ "intre 5 si 15 lei"
             , "intre 5 și 15 lei"
             , "de la 5 RON la 15 RON"
             , "intre 5 lei si 15 lei"
             , "de la 5 la 15 lei"
             , "5 - 15 roni"
             , "aproximativ 5-15 lei"
             , "aproape de 5-15 lei"
             , "cam 5-15 RON"
             ]
  , examples (under EUR 7)
             [ "sub șapte euro"
             , "mai putin de 7 EUR"
             , "mai puțin de 7 EUR"
             , "nu chiar 7€"
             , "nici macar 7 euro"
             , "mai ieftin de 7€"
             , "cel mult 7 euro"
             ]
  , examples (above Dollar 3)
            [ "mai mult de 3 dolari"
            , "peste 3 dolari"
            , "mai scump de trei dolari"
            ]
  ]
