-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Finance.GA.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Finance.Types
import Duckling.Lang
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {lang = GA}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (FinanceValue Dollar 10)
             [ "$10"
             , "10$"
             , "deich dollair"
             ]
  , examples (FinanceValue Cent 10)
             [ "deich ceinteanna"
             ]
  , examples (FinanceValue Dollar 10000)
             [ "$10,000"
             , "10K$"
             , "$10k"
             ]
  , examples (FinanceValue EUR 10000)
             [ "€10,000"
             , "10K€"
             , "€10k"
             ]
  , examples (FinanceValue USD 1.23)
             [ "USD1.23"
             ]
  , examples (FinanceValue Dollar 2.23)
             [ "2 dhollair agus 23 ceinteanna"
             , "dhá dhollair 23 ceinteanna"
             , "2 dhollair 23"
             , "dhá dhollair agus 23"
             ]
  , examples (FinanceValue EUR 20)
             [ "20€"
             , "20 euros"
             , "20 Euro"
             , "20 Euros"
             , "EUR 20"
             ]
  , examples (FinanceValue EUR 29.99)
             [ "EUR29.99"
             ]
  , examples (FinanceValue INR 20)
             [ "Rs. 20"
             , "Rs 20"
             , "20 Rúpaí"
             , "20Rs"
             , "Rs20"
             ]
  , examples (FinanceValue INR 20.43)
             [ "20 Rupees 43"
             , "fiche rúpaí 43"
             ]
  , examples (FinanceValue INR 33)
             [ "INR33"
             ]
  , examples (FinanceValue Pound 9)
             [ "£9"
             , "naoi bpunt"
             ]
  , examples (FinanceValue GBP 3.01)
             [ "GBP3.01"
             , "GBP 3.01"
             ]
  ]
