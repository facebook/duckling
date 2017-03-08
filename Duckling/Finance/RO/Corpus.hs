-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Finance.RO.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Finance.Types
import Duckling.Lang
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {lang = RO}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (FinanceValue RON 10)
             [ "10 lei"
             , "10 roni"
             , "10 RON"
             ]
  , examples (FinanceValue Cent 50)
             [ "50 bani"
             , "50 BANI"
             ]
  , examples (FinanceValue RON 10.5)
             [ "10,5 lei"
             , "10,5 ron"
             , "10 lei si 50 bani"
             ]
  , examples (FinanceValue Dollar 10)
             [ "$10"
             , "10$"
             , "zece dolari"
             , "10 dolari"
             ]
  , examples (FinanceValue Cent 10)
             [ "zece centi"
             , "zece cenți"
             ]
  , examples (FinanceValue Dollar 10000)
             [ "$10.000"
             , "$10000"
             ]
  , examples (FinanceValue USD 1.23)
             [ "USD1,23"
             ]
  , examples (FinanceValue Dollar 1)
             [ "1 dolar"
             , "un dolar"
             , "$1"
             ]
  , examples (FinanceValue Cent 2)
             [ "2 centi"
             ]
  , examples (FinanceValue Cent 23)
             [ "23 centi"
             ]
  , examples (FinanceValue Dollar 2.23)
             [ "2 dolari si 23 centi"
             , "2 dolari și 23 cenți"
             , "doi dolari si douăzeci si trei centi"
             , "doi dolari și douăzeci și trei cenți"
             , "2 dolari 23 centi"
             , "doi dolari si 23"
             , "doi dolari și 23"
             ]
  , examples (FinanceValue EUR 20)
             [ "20€"
             , "20 euro"
             , "20 Euro"
             , "EUR 20"
             ]
  , examples (FinanceValue EUR 29.99)
             [ "EUR29,99"
             ]
  , examples (FinanceValue Pound 9)
             [ "£9"
             , "noua lir"
             , "nouă lire"
             ]
  , examples (FinanceValue GBP 3.01)
             [ "GBP3,01"
             , "GBP 3,01"
             ]
  , examples (FinanceValue QAR 1)
             [ "un rial qatarian"
             , "1 rial qataria"
             ]
  , examples (FinanceValue EGP 10)
             [ "zece lira egiptiana"
             ]
  , examples (FinanceValue LBP 1)
             [ "una liră libaneză"
             ]
  , examples (FinanceValue INR 42)
             [ "42 rupii"
             , "Rs. 42"
             ]
  , examples (FinanceValue KWD 1)
             [ "un dinar kuweitian"
             ]
  , examples (FinanceValue AED 2)
             [ "2 dirhami"
             ]
  , examples (FinanceValue SAR 1)
             [ "1 rial saudit"
             , "un rial saudi"
             ]
  ]
