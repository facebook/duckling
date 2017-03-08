-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Finance.SV.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Finance.Types
import Duckling.Lang
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {lang = SV}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (FinanceValue Dollar 10)
             [ "$10"
             , "10$"
             , "tio dollar"
             ]
  , examples (FinanceValue Cent 10)
             [ "tio öre"
             ]
  , examples (FinanceValue Dollar 10000)
             [ "$10.000"
             , "10K$"
             , "$10k"
             ]
  , examples (FinanceValue USD 1.23)
             [ "USD1,23"
             ]
  , examples (FinanceValue SEK 10)
             [ "10kronor"
             , "10kr"
             , "10 kr"
             , "tio kronor"
             , "10 SEK"
             ]
  , examples (FinanceValue SEK 2.23)
             [ "2 kronor och 23 öre"
             , "två kronor 23 öre"
             , "två kronor och 23 öre"
             ]
  , examples (FinanceValue EUR 20)
             [ "20€"
             , "20 euro"
             , "20 Euro"
             , "20 Euros"
             , "EUR 20"
             ]
  , examples (FinanceValue EUR 29.99)
             [ "EUR29,99"
             ]
  , examples (FinanceValue INR 20)
             [ "Rs. 20"
             , "Rs 20"
             , "20 Rupees"
             , "20Rs"
             , "Rs20"
             ]
  , examples (FinanceValue INR 20.43)
             [ "20 Rupees 43"
             , "tjugo rupees 43"
             ]
  , examples (FinanceValue INR 33)
             [ "INR33"
             ]
  , examples (FinanceValue Pound 9)
             [ "£9"
             , "nio pund"
             ]
  , examples (FinanceValue GBP 3.01)
             [ "GBP3,01"
             , "GBP 3,01"
             ]
  , examples (FinanceValue NOK 10)
             [ "10 norska kronor"
             , "10 nkr"
             ]
  ]
