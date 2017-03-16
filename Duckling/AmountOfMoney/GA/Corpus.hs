-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.GA.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.AmountOfMoney.Types
import Duckling.Lang
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {lang = GA}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (AmountOfMoneyValue Dollar 10)
             [ "$10"
             , "10$"
             , "deich dollair"
             ]
  , examples (AmountOfMoneyValue Cent 10)
             [ "deich ceinteanna"
             ]
  , examples (AmountOfMoneyValue Dollar 10000)
             [ "$10,000"
             , "10K$"
             , "$10k"
             ]
  , examples (AmountOfMoneyValue EUR 10000)
             [ "€10,000"
             , "10K€"
             , "€10k"
             ]
  , examples (AmountOfMoneyValue USD 1.23)
             [ "USD1.23"
             ]
  , examples (AmountOfMoneyValue Dollar 2.23)
             [ "2 dhollair agus 23 ceinteanna"
             , "dhá dhollair 23 ceinteanna"
             , "2 dhollair 23"
             , "dhá dhollair agus 23"
             ]
  , examples (AmountOfMoneyValue EUR 20)
             [ "20€"
             , "20 euros"
             , "20 Euro"
             , "20 Euros"
             , "EUR 20"
             ]
  , examples (AmountOfMoneyValue EUR 29.99)
             [ "EUR29.99"
             ]
  , examples (AmountOfMoneyValue INR 20)
             [ "Rs. 20"
             , "Rs 20"
             , "20 Rúpaí"
             , "20Rs"
             , "Rs20"
             ]
  , examples (AmountOfMoneyValue INR 20.43)
             [ "20 Rupees 43"
             , "fiche rúpaí 43"
             ]
  , examples (AmountOfMoneyValue INR 33)
             [ "INR33"
             ]
  , examples (AmountOfMoneyValue Pound 9)
             [ "£9"
             , "naoi bpunt"
             ]
  , examples (AmountOfMoneyValue GBP 3.01)
             [ "GBP3.01"
             , "GBP 3.01"
             ]
  ]
