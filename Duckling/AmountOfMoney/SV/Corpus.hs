-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.SV.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.AmountOfMoney.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale SV Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Dollar 10)
             [ "$10"
             , "10$"
             , "tio dollar"
             ]
  , examples (simple Cent 10)
             [ "tio öre"
             ]
  , examples (simple Dollar 10000)
             [ "$10.000"
             , "10K$"
             , "$10k"
             ]
  , examples (simple USD 1.23)
             [ "USD1,23"
             ]
  , examples (simple SEK 10)
             [ "10kronor"
             , "10kr"
             , "10 kr"
             , "tio kronor"
             , "10 SEK"
             ]
  , examples (simple SEK 2.23)
             [ "2 kronor och 23 öre"
             , "två kronor 23 öre"
             , "två kronor och 23 öre"
             ]
  , examples (simple EUR 20)
             [ "20€"
             , "20 euro"
             , "20 Euro"
             , "20 Euros"
             , "EUR 20"
             ]
  , examples (simple EUR 29.99)
             [ "EUR29,99"
             ]
  , examples (simple INR 20)
             [ "Rs. 20"
             , "Rs 20"
             , "20 Rupees"
             , "20Rs"
             , "Rs20"
             ]
  , examples (simple INR 20.43)
             [ "20 Rupees 43"
             , "tjugo rupees 43"
             ]
  , examples (simple INR 33)
             [ "INR33"
             ]
  , examples (simple Pound 9)
             [ "£9"
             , "nio pund"
             ]
  , examples (simple GBP 3.01)
             [ "GBP3,01"
             , "GBP 3,01"
             ]
  , examples (simple NOK 10)
             [ "10 norska kronor"
             , "10 nkr"
             ]
  ]
