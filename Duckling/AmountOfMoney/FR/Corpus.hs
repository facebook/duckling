-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.FR.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.AmountOfMoney.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale FR Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Dollar 10)
             [ "$10"
             , "10$"
             , "dix dollars"
             ]
  , examples (simple Dollar 10000)
             [ "$10.000"
             , "10K$"
             , "$10k"
             , "10 000 dollars"
             , "10 000,00 $"
             ]
  , examples (simple USD 1.23)
             [ "USD1,23"
             , "1 USD et 23 centimes"
             , "1 USD 23 cents"
             ]
  , examples (simple EUR 20)
             [ "20€"
             , "20 euros"
             , "20 Euro"
             , "20 Euros"
             , "EUR 20"
             ]
  , examples (simple EUR 29.99)
             [ "EUR29,99"
             ]
  , examples (simple Unnamed 3)
             [ "3 balles"
             , "à peu près 3 pouloutes"
             ]
  , examples (simple Pound 9)
             [ "exactement £9"
             , "neuf pounds"
             , "quasi neuf livres"
             ]
  , examples (simple GBP 3.01)
             [ "GBP3,01"
             , "GBP 3,01"
             ]
  , examples (simple ILS 10)
             [ "dix shekels"
             ]
  , examples (simple Rial 10)
             [ "dix rials"
             ]
  , examples (simple Riyal 10)
             [ "dix riyals"
             ]
  , examples (between EUR (10, 20))
             [ "entre 10 et 20 euro"
             , "entre 10 euro et 20 euro"
             , "10 - 20 euro"
             , "10 euro - 20 euro"
             ]
  , examples (above EUR 10)
             [ "au moins 10 euro"
             , "plus que 10 euros"
             , "plus de 10 euros"
             ]
  , examples (under Dollar 10)
             [ "en-dessous de 10 dollars"
             , "moins de 10 dollars"
             , "pas plus de 10 dollars"
             ]
  ]
