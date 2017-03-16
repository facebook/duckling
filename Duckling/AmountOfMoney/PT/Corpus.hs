-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.PT.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.AmountOfMoney.Types
import Duckling.Lang
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {lang = PT}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (AmountOfMoneyValue Dollar 10)
             [ "$10"
             , "10$"
             , "dez dolares"
             ]
  , examples (AmountOfMoneyValue Dollar 10000)
             [ "$10.000"
             , "10K$"
             , "$10k"
             ]
  , examples (AmountOfMoneyValue USD 1.23)
             [ "USD1,23"
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
  , examples (AmountOfMoneyValue Pound 9)
             [ "£9"
             , "nove libras"
             ]
  , examples (AmountOfMoneyValue GBP 3.01)
             [ "GBP3,01"
             , "GBP 3,01"
             ]
  , examples (AmountOfMoneyValue PTS 15)
             [ "15 Pt"
             , "15pta"
             , "15Ptas"
             ]
  , examples (AmountOfMoneyValue BRL 15)
             [ "15 reais"
             , "15reais"
             , "15 Reais"
             , "BRL 15"
             ]
  , examples (AmountOfMoneyValue BRL 2.0)
             [ "R$2,00"
             , "R$ 2,00"
             , "2,00 reais"
             ]
  ]
