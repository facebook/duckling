-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


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
             [ "$10.000"
             , "10K$"
             , "$10k"
             ]
  , examples (simple USD 1.23)
             [ "USD1,23"
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
             [ "10.000 đồng"
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
             [ "GBP3,01"
             , "GBP 3,01"
             ]
  , examples (simple AED 1)
             [ "1 AED."
             , "1 dirham"
             ]
  , examples (between VND (1000, 2000))
             [ "giữa 1000 và 2000 đồng"
             , "giữa 1000 đồng và 2000 đồng"
             , "từ 1000 đồng đến 2000 đồng"
             , "từ 1000 VNĐ tới 2000 VNĐ"
             , "từ 1000 đến 2000 đồng"
             , "khoảng 1000-2000 đồng"
             , "khoảng chừng từ 1000 đến 2000 đồng"
             , "tầm khoảng 1000 tới 2000 đồng"
             , "xấp xỉ VND1000-VND2000"
             , "1000-2000 đồng"
             ]
  , examples (between Dollar (1.1, 1.3))
             [ "giữa 1,1 và 1,3 đô la"
             , "từ 1 phẩy 1 đến một chấm ba đô la"
             ]
  , examples (under VND 7000)
             [ "dưới bảy ngàn đồng"
             , "ít hơn bảy nghìn đồng"
             , "kém hơn 7k đồng"
             , "không tới 7000 đồng"
             , "không cao hơn 7000 đồng"
             , "không hơn 7.000 đồng"
             , "không quá 7.000 đồng"
             , "từ 7000 đồng trở xuống"
             ]
  , examples (above Dollar 1.42)
             [ "nhiều hơn 1 đô la và bốn mươi hai xen"
             , "ít nhất $1,42"
             , "hơn 1,42 đô la"
             , "trên một đô la và 42 xu"
             , "không ít hơn 1,42 đô la"
             , "cao hơn $1,42"
             , "từ $1,42 trở lên"
             ]
  ]
