-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.NL.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.AmountOfMoney.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale NL Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Dollar 1)
             [ "$1"
             , "1 dollar"
             ]
  , examples (simple Dollar 10)
             [ "$10"
             , "$ 10"
             , "10$"
             , "10 dollars"
             , "tien dollar"
             ]
  , examples (simple Cent 10)
             [ "10 cent"
             , "tien pennies"
             , "tien cents"
             , "10 c"
             , "10¢"
             ]
  , examples (simple Dollar 10000)
             [ "$10K"
             , "10k$"
             , "$10000"
             , "10000,00 $"
             ]
  , examples (simple USD 3.14)
             [ "USD3,14"
             , "3,14US$"
             , "US$ 3,14"
             ]
  , examples (simple EUR 20)
             [ "20\x20ac"
             , "20 euros"
             , "20 Euro"
             , "20 Euros"
             , "EUR 20"
             , "EUR 20,0"
             , "20€"
             , "20 €ur"
             ]
  , examples (simple Dollar 20.43)
             [ "$20 en 43c"
             , "$20,43"
             , "20 dollar 43c"
             , "20 dollars 43 cents"
             , "20 dollar 43"
             ]
  , examples (simple GBP 3.01)
             [ "GBP3,01"
             , "GBP 3,01"
             , "3 GBP een penny"
             ]
  , examples (simple Unnamed 42)
             [ "42 bucks"
             , "ongeveer twee-en-veertig bucks"
             , "precies 42 bucks"
             ]
  , examples (between Dollar (10, 20))
             [ "vanaf 10 dollars tot 20 dollars"
             , "rond de 10-20 dollars"
             , "tussen 10 dollars en 20 dollars"
             , "ongeveer $10-$20"
             , "10-20 dollars"
             ]
  , examples (between Dollar (1.1, 1.3))
             [ "tussen 1,1 dollar en 1,3 dollars"
             , "van 1,1$ en 1,3 dollars"
             ]
  , examples (under EUR 7)
             [ "minder dan 7 euros"
             , "minder dan zeven EUR"
             , "lager dan 7€"
             ]
  , examples (above Dollar 1.42)
             [ "meer dan 1 dollar en 42 cents"
             , "minstens $1,42"
             , "boven de 1,42 dollars"
             , "boven de 1 dollar en 42 cents"
             ]
  ]
