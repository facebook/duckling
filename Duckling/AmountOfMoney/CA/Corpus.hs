-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.CA.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.AmountOfMoney.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale CA Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Dollar 10)
             [ "$10"
             , "10$"
             , "deu dolars"
             , "deu dòlars"
             ]
  , examples (simple Dollar 10000)
             [ "$10.000"
             , "10K$"
             , "$10k"
             ]
  , examples (simple USD 1.23)
             [ "USD1,23"
             ]
  , examples (simple EUR 20)
             [ "20€"
             , "20 euros"
             , "20 Euro"
             , "20 Euros"
             , "EUR 20"
             , "vint euros"
             ]
  , examples (simple EUR 29.99)
             [ "EUR29,99"
             , "29,99 euros"
             , "29,99 €"
             , "29,99€"
             ]
  , examples (simple Pound 9)
             [ "£9"
             , "nou lliures"
             , "9 lliures"
             ]
  ,examples (simple Pound 1)
             [ "£1"
             , "una lliura"
             , "1 lliura"
             ]
  , examples (simple GBP 3.01)
             [ "GBP3,01"
             , "GBP 3,01"
             , "3 gbp 1 cèntims"
             , "3 gbp i 1 cèntims"
             ]
  , examples (simple PTS 15)
             [ "15 Pt"
             , "15pta"
             , "15Ptas"
             ]
  , examples (between Dollar (10, 20))
             [ "entre 10 i 20 dòlars"
             , "des de $10 fins a $20"
             , "10$ - 20 dolars"
             , "10 - 20 $"
             ]
  , examples (under Dollar 10)
             [ "menys de 10 dolars"
             , "no més de 10$"
             ]
  , examples (above Dollar 10)
             [ "no menys de 10 dòlars"
             , "més de 10$"
             ]
  ]
