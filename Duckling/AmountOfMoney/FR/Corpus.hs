-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.FR.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.AmountOfMoney.Types
import Duckling.Lang
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {lang = FR}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (AmountOfMoneyValue Dollar 10)
             [ "$10"
             , "10$"
             , "dix dollars"
             ]
  , examples (AmountOfMoneyValue Dollar 10000)
             [ "$10.000"
             , "10K$"
             , "$10k"
             ]
  , examples (AmountOfMoneyValue USD 1.23)
             [ "USD1,23"
             , "1 USD et 23 centimes"
             , "1 USD 23 cents"
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
  , examples (AmountOfMoneyValue Unnamed 3)
             [ "3 balles"
             , "à peu près 3 pouloutes"
             ]
  , examples (AmountOfMoneyValue Pound 9)
             [ "exactement £9"
             , "neuf pounds"
             , "quasi neuf livres"
             ]
  , examples (AmountOfMoneyValue GBP 3.01)
             [ "GBP3,01"
             , "GBP 3,01"
             ]
  ]
