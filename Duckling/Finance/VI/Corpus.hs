-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Finance.VI.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Finance.Types
import Duckling.Lang
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {lang = VI}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (FinanceValue Dollar 10)
             [ "$10"
             , "10$"
             , "mười đô"
             , "mười đô la"
             , "mười đô mỹ"
             ]
  , examples (FinanceValue Cent 10)
             [ "mười xen"
             , "mười xu"
             ]
  , examples (FinanceValue Dollar 10000)
             [ "$10,000"
             , "10K$"
             , "$10k"
             ]
  , examples (FinanceValue USD 1.23)
             [ "USD1.23"
             ]
  , examples (FinanceValue Dollar 2.23)
             [ "2 đô la và 23 xen"
             , "hai đô la và 23 xen"
             , "2 đô 23 xu"
             , "hai đô 23"
             , "2 chấm 23 đô la"
             , "hai phẩy 23 đô"
             ]
  , examples (FinanceValue VND 10)
             [ "mười đồng"
             ]
  , examples (FinanceValue VND 10000)
             [ "10,000 đồng"
             , "10K đồng"
             , "10k đồng"
             ]
  , examples (FinanceValue VND 1000)
             [ "1000 VNĐ"
             , "VN$1000"
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
             , "20 Rupees"
             , "20Rs"
             , "Rs20"
             ]
  , examples (FinanceValue INR 20.43)
             [ "20 Rupees 43"
             , "hai mươi rupees 43"
             , "hai mươi rupees 43 xen"
             ]
  , examples (FinanceValue INR 33)
             [ "INR33"
             ]
  , examples (FinanceValue Pound 9)
             [ "£9"
             , "chín pounds"
             ]
  , examples (FinanceValue GBP 3.01)
             [ "GBP3.01"
             , "GBP 3.01"
             ]
  , examples (FinanceValue AED 1)
             [ "1 AED."
             , "1 dirham"
             ]
  ]
