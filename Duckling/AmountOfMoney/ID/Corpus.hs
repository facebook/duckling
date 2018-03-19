-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.ID.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.AmountOfMoney.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale ID Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Dollar 10)
             [ "$10"
             , "10$"
             , "sepuluh dolar"
             ]
  , examples (simple Dollar 10000)
             [ "$10.000"
             , "10K$"
             , "$10k"
             ]
  , examples (simple USD 1.23)
             [ "USD1,23"
             ]
  , examples (simple Dollar 2)
             [ "2 dola"
             , "dua dolar"
             ]
  , examples (simple EUR 20)
             [ "20€"
             , "20 euros"
             , "20 Euro"
             , "20 Euros"
             , "EUR 20"
             , "dua puluh Euro"
             ]
  , examples (simple EUR 29.99)
             [ "EUR29,99"
             ]
  , examples (simple IDR 315)
             [ "Rp. 315,00"
             ]
  , examples (simple IDR 20)
             [ "Rp 20"
             , "20 Rupiah"
             , "20Rp"
             , "Rp20"
             ]
  , examples (simple IDR 33000)
             [ "IDR33000"
             , "IDR 33.000"
             ]
  , examples (simple Pound 9)
             [ "£9"
             , "sembilan pound"
             , "sembilan pound sterling"
             ]
  , examples (simple GBP 3.01)
             [ "GBP3,01"
             , "GBP 3,01"
             ]
  , examples (simple JPY 10)
             [ "10 yen"
             , "10¥"
             , "10 ¥."
             ]
  ]
