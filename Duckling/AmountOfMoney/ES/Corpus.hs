-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.ES.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.AmountOfMoney.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale ES Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Dollar 10)
             [ "$10"
             , "10$"
             , "diez dollars"
             , "diez dólares"
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
             , "nueve pounds"
             , "9 libras"
             ]
  , examples (simple GBP 3.01)
             [ "GBP3,01"
             , "GBP 3,01"
             , "3 gbp 1 centavo"
             , "3 gbp y 1 centavo"
             ]
  , examples (simple PTS 15)
             [ "15 Pt"
             , "15pta"
             , "15Ptas"
             ]
  ]
