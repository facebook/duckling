-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.IT.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.AmountOfMoney.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale IT Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Dollar 10)
             [ "$10"
             , "10$"
             , "dieci dollari"
             ]
  , examples (simple Dollar 10000)
             [ "$10.000"
             , "10K$"
             , "$10k"
             , "10 000 dollari"
             , "10 000,00 $"
             ]
  , examples (simple USD 1.23)
             [ "USD1,23"
             , "1 USD e 23 centesimi"
             , "1 USD 23 centesimi"
             ]
  , examples (simple EUR 20)
             [ "20€"
             , "20 euro"
             , "20 Euro"
             , "20 Euro"
             , "EUR 20"
             ]
  , examples (simple EUR 29.99)
             [ "EUR29,99"
             ]
  , examples (simple Pound 9)
             [ "nove sterline"
             , "quasi nove sterline"
             ]
  , examples (simple GBP 3.01)
             [ "GBP3,01"
             , "GBP 3,01"
             ]
  , examples (between EUR (10, 20))
             [ "tra 10 e 20 euro"
             , "tra 10 euro e 20 euro"
             , "10 - 20 euro"
             , "10 euro - 20 euro"
             ]
  , examples (above EUR 10)
             [ "almeno 10 euro"
             , "più di 10 euro"
             , "più di 10 euro"
             ]
  , examples (under Dollar 10)
             [ "meno di 10 dollari"
             , "non più di 10 dollari"
             ]
  ]
