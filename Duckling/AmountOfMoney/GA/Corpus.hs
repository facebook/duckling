-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.GA.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.AmountOfMoney.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale GA Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Dollar 10)
             [ "$10"
             , "10$"
             , "deich dollair"
             ]
  , examples (simple Cent 10)
             [ "deich ceinteanna"
             ]
  , examples (simple Dollar 10000)
             [ "$10,000"
             , "10K$"
             , "$10k"
             ]
  , examples (simple EUR 10000)
             [ "€10,000"
             , "10K€"
             , "€10k"
             ]
  , examples (simple USD 1.23)
             [ "USD1.23"
             ]
  , examples (simple Dollar 2.23)
             [ "2 dhollair agus 23 ceinteanna"
             , "dhá dhollair 23 ceinteanna"
             , "2 dhollair 23"
             , "dhá dhollair agus 23"
             ]
  , examples (simple EUR 20)
             [ "20€"
             , "20 euros"
             , "20 Euro"
             , "20 Euros"
             , "EUR 20"
             ]
  , examples (simple EUR 29.99)
             [ "EUR29.99"
             ]
  , examples (simple INR 20)
             [ "Rs. 20"
             , "Rs 20"
             , "20 Rúpaí"
             , "20Rs"
             , "Rs20"
             ]
  , examples (simple INR 20.43)
             [ "20 Rupees 43"
             , "fiche rúpaí 43"
             ]
  , examples (simple INR 33)
             [ "INR33"
             ]
  , examples (simple Pound 9)
             [ "£9"
             , "naoi bpunt"
             ]
  , examples (simple GBP 3.01)
             [ "GBP3.01"
             , "GBP 3.01"
             ]
  ]
