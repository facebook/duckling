-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


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
             [ "2 dolar"
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
  , examples (between Dollar(10, 20))
             [ "antara 10 sampai 20 dolar"
             , "dari 10 dolar sampai 20 dolar"
             , "sekitar 10 sampai 20 dolar"
             , "antara 10 hingga 20 dolar"
             , "sekitar 10 dolar hingga 20 dolar"
             , "kira-kira 10-20 dolar"
             , "10-20 dolar"
             ]
  , examples (between IDR(1000, 10000))
             [ "antara Rp1000 sampai Rp10000"
             , "dari 1000 Rupiah hingga 10000 Rupiah"
             , "sekitar 1000 sampai 10000 Rupiah"
             , "1000-10000 Rupiah"
             , "kira-kira Rp1000 sampai Rp10000"
             , "dari Rp1.000 sampai Rp10.000"
             , "antara 1.000 hingga 10.000 Rupiah"
             , "kira-kira 1.000-10.000 Rupiah"
             , "sekitar 1.000 hingga 10.000 Rupiah"
             ]
  , examples (under EUR 7)
             [ "kurang dari 7 euro"
             , "di bawah EUR 7"
             , "tidak sampai 7 euro"
             , "lebih murah dari EUR 7"
             ]
  , examples (under IDR 500)
             [ "kurang dari 500 Rupiah"
             , "tidak sampai Rp500"
             , "di bawah Rp 500"
             , "lebih murah dari 500 rupiah"
             ]
  , examples (above USD 1.42)
             [ "di atas USD1,42"
             , "lebih dari USD1,42"
             , "melebihi USD1,42"
             , "melewati USD1,42"
             ]
  , examples (above IDR 33000)
             [ "di atas 33.000 Rupiah"
             , "lebih dari Rp33000"
             , "melebihi Rp33.000"
             , "melewati 33000 rupiah"
             ]
  ]
