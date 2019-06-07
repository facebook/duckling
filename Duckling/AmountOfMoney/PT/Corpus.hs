-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.PT.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.AmountOfMoney.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale PT Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Dollar 10)
             [ "$10"
             , "10$"
             , "dez dolares"
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
             ]
  , examples (simple EUR 29.99)
             [ "EUR29,99"
             ]
  , examples (simple Pound 9)
             [ "£9"
             , "nove libras"
             ]
  , examples (simple GBP 3.01)
             [ "GBP3,01"
             , "GBP 3,01"
             ]
  , examples (simple PTS 15)
             [ "15 Pt"
             , "15pta"
             , "15Ptas"
             ]
  , examples (simple BRL 15)
             [ "15 reais"
             , "15reais"
             , "15 Reais"
             , "BRL 15"
             ]
  , examples (simple BRL 2.0)
             [ "R$2,00"
             , "R$ 2,00"
             , "2,00 reais"
             ]
  ]
