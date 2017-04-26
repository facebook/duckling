-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.

{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.HR.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Lang
import Duckling.AmountOfMoney.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {lang = HR}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (AmountOfMoneyValue Dollar 10)
             [ "$10"
             , "10$"
             , "deset dolara"
             ]
  , examples (AmountOfMoneyValue Cent 10)
             [ "deset centa"
             ]
  , examples (AmountOfMoneyValue Dollar 10000)
             [ "$10.000"
             , "10K$"
             , "$10k"
             ]
  , examples (AmountOfMoneyValue USD 1.23)
             [ "USD1,23"
             ]
  , examples (AmountOfMoneyValue Dollar 2.23)
             [ "2 dolara i 23 centa"
             , "dva dolara 23 centa"
             , "2 dolara 23"
             , "dva dolara i 23"
             ]
  , examples (AmountOfMoneyValue HRK 2.23)
             [ "2 kune i 23 lipe"
             , "dvije kune 23 lipe"
             , "2 kune 23"
             , "dvije kune i 23"
             ]
  , examples (AmountOfMoneyValue HRK 100)
             [ "100 kuna"
             , "sto kuna"
             ]
  , examples (AmountOfMoneyValue HRK 200)
             [ "200 kuna"
             , "dvije stotine kuna"
             , "dvjesto kuna"
             , "dvjesta kuna"
             ]
  , examples (AmountOfMoneyValue EUR 20)
             [ "20€"
             , "20 euros"
             , "20 Euro"
             , "20 Euros"
             , "EUR 20"
             ]
  , examples (AmountOfMoneyValue EUR 29.99)
             [ "EUR29,99"
             ]
  , examples (AmountOfMoneyValue INR 20)
             [ "Rs. 20"
             , "Rs 20"
             , "20 Rupija"
             , "20Rs"
             , "Rs20"
             ]
  , examples (AmountOfMoneyValue INR 20.43)
             [ "20 Rupija 43"
             , "dvadeset rupija 43"
             ]
  , examples (AmountOfMoneyValue INR 33)
             [ "INR33"
             ]
  , examples (AmountOfMoneyValue Pound 9)
             [ "£9"
             , "devet funti"
             ]
  , examples (AmountOfMoneyValue GBP 3.01)
             [ "GBP3,01"
             , "GBP 3,01"
             ]
  ]
