-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.NB.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.AmountOfMoney.Types
import Duckling.Lang
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {lang = NB}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (AmountOfMoneyValue Dollar 10)
             [ "$10"
             , "10$"
             , "ti dollar"
             ]
  , examples (AmountOfMoneyValue Cent 10)
             [ "ti øre"
             ]
  , examples (AmountOfMoneyValue Dollar 10000)
             [ "$10.000"
             , "10K$"
             , "$10k"
             ]
  , examples (AmountOfMoneyValue USD 1.23)
             [ "USD1,23"
             ]
  , examples (AmountOfMoneyValue NOK 10)
             [ "10kroner"
             , "10kr"
             , "ti kroner"
             , "10 NOK"
             ]
  , examples (AmountOfMoneyValue NOK 2.23)
             [ "2 kroner og 23 øre"
             , "to kroner 23 øre"
             , "to kroner og 23 øre"
             , "to kr 23"
             ]
  , examples (AmountOfMoneyValue EUR 20)
             [ "20€"
             , "20 euro"
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
             , "20 Rupees"
             , "20Rs"
             , "Rs20"
             , "INR20"
             ]
  , examples (AmountOfMoneyValue INR 20.43)
             [ "20 Rupees 43"
             , "tjue rupees 43"
             , "tjue rupees 43¢"
             ]
  , examples (AmountOfMoneyValue Pound 9)
             [ "£9"
             , "ni pund"
             ]
  , examples (AmountOfMoneyValue GBP 3.01)
             [ "GBP3,01"
             , "GBP 3,01"
             ]
  ]
