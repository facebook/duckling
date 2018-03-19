-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.RO.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.AmountOfMoney.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale RO Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple RON 10)
             [ "10 lei"
             , "10 roni"
             , "10 RON"
             ]
  , examples (simple Cent 50)
             [ "50 bani"
             , "50 BANI"
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
             [ "23 centi"
             ]
  , examples (simple Dollar 2.23)
             [ "2 dolari si 23 centi"
             , "2 dolari și 23 cenți"
             , "doi dolari si douăzeci si trei centi"
             , "doi dolari și douăzeci și trei cenți"
             , "2 dolari 23 centi"
             , "doi dolari si 23"
             , "doi dolari și 23"
             ]
  , examples (simple EUR 20)
             [ "20€"
             , "20 euro"
             , "20 Euro"
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
             [ "42 rupii"
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
  ]
