-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.VI.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.AmountOfMoney.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale VI Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Dollar 10)
             [ "$10"
             , "10$"
             , "mười đô"
             , "mười đô la"
             , "mười đô mỹ"
             ]
  , examples (simple Cent 10)
             [ "mười xen"
             , "mười xu"
             ]
  , examples (simple Dollar 10000)
             [ "$10,000"
             , "10K$"
             , "$10k"
             ]
  , examples (simple USD 1.23)
             [ "USD1.23"
             ]
  , examples (simple Dollar 2.23)
             [ "2 đô la và 23 xen"
             , "hai đô la và 23 xen"
             , "2 đô 23 xu"
             , "hai đô 23"
             , "2 chấm 23 đô la"
             , "hai phẩy 23 đô"
             ]
  , examples (simple VND 10)
             [ "mười đồng"
             ]
  , examples (simple VND 10000)
             [ "10,000 đồng"
             , "10K đồng"
             , "10k đồng"
             ]
  , examples (simple VND 1000)
             [ "1000 VNĐ"
             , "VN$1000"
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
             , "20 Rupees"
             , "20Rs"
             , "Rs20"
             ]
  , examples (simple INR 20.43)
             [ "20 Rupees 43"
             , "hai mươi rupees 43"
             , "hai mươi rupees 43 xen"
             ]
  , examples (simple INR 33)
             [ "INR33"
             ]
  , examples (simple Pound 9)
             [ "£9"
             , "chín pounds"
             ]
  , examples (simple GBP 3.01)
             [ "GBP3.01"
             , "GBP 3.01"
             ]
  , examples (simple AED 1)
             [ "1 AED."
             , "1 dirham"
             ]
  ]
