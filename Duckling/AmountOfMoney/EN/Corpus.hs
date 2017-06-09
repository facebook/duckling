-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.EN.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.AmountOfMoney.Types
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (AmountOfMoneyValue Dollar 10)
             [ "$10"
             , "$ 10"
             , "10$"
             , "10 dollars"
             , "ten dollars"
             ]
  , examples (AmountOfMoneyValue Cent 10)
             [ "10 cent"
             , "ten pennies"
             , "ten cents"
             , "10 c"
             , "10¢"
             ]
  , examples (AmountOfMoneyValue Dollar 1e4)
             [ "$10K"
             , "10k$"
             , "$10,000"
             ]
  , examples (AmountOfMoneyValue USD 3.14)
             [ "USD3.14"
             , "3.14US$"
             , "US$ 3.14"
             ]
  , examples (AmountOfMoneyValue EUR 20)
             [ "20\x20ac"
             , "20 euros"
             , "20 Euro"
             , "20 Euros"
             , "EUR 20"
             , "EUR 20.0"
             , "20€"
             , "20 €ur"
             ]
  , examples (AmountOfMoneyValue Pound 10)
             [ "\x00a3\&10"
             , "ten pounds"
             ]
  , examples (AmountOfMoneyValue INR 20)
             [ "Rs. 20"
             , "Rs 20"
             , "20 Rupees"
             , "20Rs"
             , "Rs20"
             ]
  , examples (AmountOfMoneyValue INR 20.43)
             [ "20 Rupees 43"
             , "twenty rupees 43"
             ]
  , examples (AmountOfMoneyValue Dollar 20.43)
             [ "$20 and 43c"
             , "$20 43"
             , "20 dollar 43c"
             , "20 dollars 43 cents"
             , "twenty dollar 43 cents"
             , "20 dollar 43"
             , "twenty dollar and 43"
             ]
  , examples (AmountOfMoneyValue GBP 3.01)
             [ "GBP3.01"
             , "GBP 3.01"
             , "3 GBP 1 pence"
             ]
  , examples (AmountOfMoneyValue Unnamed 42)
             [ "42 bucks"
             , "around 42 bucks"
             , "exactly 42 bucks"
             ]
  , examples (AmountOfMoneyValue KWD 42)
             [ "42 KWD"
             , "42 kuwaiti Dinar"
             ]
  , examples (AmountOfMoneyValue LBP 42)
             [ "42 LBP"
             , "42 Lebanese Pounds"
             ]
  , examples (AmountOfMoneyValue EGP 42)
             [ "42 EGP"
             , "42 egyptianpound"
             ]
  , examples (AmountOfMoneyValue QAR 42)
             [ "42 QAR"
             , "42 qatari riyals"
             ]
  , examples (AmountOfMoneyValue SAR 42)
             [ "42 SAR"
             , "42 Saudiriyal"
             ]
  , examples (AmountOfMoneyValue MYR 42)
             [ "42 MYR"
             , "42 RM"
             , "RM 42"
             , "MYR 42"
             , "42MYR"
             , "42RM"
             , "RM42"
             , "MYR42"
             , "ringgit 42"
             , "42 ringgit"
             , "42 malaysia ringgit"
             , "malaysia ringgit 42"
             , "42 malaysian ringgit"
             , "malaysian ringgit 42"
             , "42 malaysia ringgits"
             , "malaysia ringgits 42"
             , "42 malaysian ringgits"
             , "malaysian ringgits 42"
             ]
  , examples (AmountOfMoneyValue MYR 20.43)
             [ "20 ringgit and 43c"
             , "20 ringgit and 43 sen"
             , "twenty ringgit 43 sens"
             , "20 ringgit 43"
             , "twenty ringgit and 43"
             ]
  ]
