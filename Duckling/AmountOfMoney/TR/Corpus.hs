
-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.TR.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.AmountOfMoney.Types
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Dollar 1)
             [ "1$"
             , "bir dollar"
             , "1 dollar"
             ]
  , examples (simple Dollar 10)
             [ "10$"
             , "10 $"
             , "10$"
             , "10 dollar"
             , "on dollar"
             ]
  , examples (simple Cent 10)
             [ "10 sent"
             , "on peni"
             , "on sent"
             , "10 c"
             , "10¢"
             ]
  , examples (simple Dollar 1e4)
             [ "$10K"
             , "10k$"
             , "$10,000"
             ]
  , examples (simple USD 3.14)
             [ "3.14USD"
             , "3.14US$"
             , "3.14 US$"
             ]
  , examples (simple EUR 20)
             [ "20\x20ac"
             , "20 euro"
             , "20 Euro"
             , "20 Euro"
             , "20 EUR"
             , "20.0 EUR"
             , "20€"
             , "20 €ur"
             ]
  , examples (simple Pound 10)
             [ "\x00a3\&10"
             , "ten pound"
             ]
  , examples (simple INR 20)
             [ "Rs. 20"
             , "Rs 20"
             , "20 Rupi"
             , "20Rs"
             , "Rs20"
             ]
  , examples (simple INR 20.43)
             [ "20 Rupi 43"
             , "yirmi rupi 43"
             ]
  , examples (simple Dollar 20.43)
             [ "20$ 43c"
             , "20$ 43 sent"
             , "20 dollar 43c"
             , "20 dollar 43 sent"
             , "yirmi dollar 43 sent"
             , "20 dollar 43"
             , "yirmi dollar 43 sent"
             ]
  , examples (simple GBP 3.01)
             [ "3.01GBP"
             , "3.01GBP"
             , "3 GBP 1 pence"
             ]
  , examples (simple Unnamed 42)
             [ "42 papel"
             , "yaklaşık 42 papel"
             , "42 papel"
             ]
  , examples (simple KWD 42)
             [ "42 KWD"
             , "42 kuwaiti Dinar"
             ]
  , examples (simple LBP 42)
             [ "42 LBP"
             , "42 Lübnan Poundu"
             ]
  , examples (simple EGP 42)
             [ "42 EGP"
             , "42 mısır poundu"
             ]
  , examples (simple QAR 42)
             [ "42 QAR"
             , "42 katar riyali"
             ]
  , examples (simple SAR 42)
             [ "42 SAR"
             , "42 suudi arabistan riyali"
             ]
  , examples (simple TRY 1)
               [ "₺1"
               , "1 tl"
               , "1 TRY"
               , "1tl"
               , "1 Lira"
               , "1 Türk lirası"
               ]
  , examples (simple TRY 10)
               [ "₺10"
               , "10 tl"
               , "10tl"
               , "10 Lira"
               , "10 Türk lirası"
               ]
  , examples (simple MYR 42)
             [ "42 MYR"
             , "42 RM"
             , "RM 42"
             , "MYR 42"
             , "42MYR"
             , "42RM"
             , "RM42"
             , "MYR42"
             , "42 ringgit"
             , "42 ringgit"
             , "42 malaysia ringgit"
             , "malaysia ringgit 42"
             , "42 malezya ringgiti"
             , "42 malezya ringgit"
             , "42 malezya ringgiti"
             , "malezya ringgiti 42"
             ]
  , examples (simple MYR 20.43)
             [ "20 ringgit and 43c"
             , "20 ringgit and 43 sen"
             , "twenty ringgit 43 sens"
             , "20 ringgit 43"
             , "yirmi ringgit and 43"
             ]
  , examples (between Dollar (10, 20))
             [ "10 ila 20 dolar arasında"
             , "10 dolardan 20 dolara"
             , "10-20 dolar civarında"
             , "10 ila 20 dolardan"
             , "yaklaşık 10- 20 $"
             , "10-20 dollar"
             ]
  , examples (under EUR 7)
             [ "yedi avronun altında"
             , "7 EUR'den az"
             , "7 € 'dan daha düşük"
             ]
  , examples (above Dollar 1.42)
             [ "1 dolar kırk iki sentten fazla"
             , "en az 1,42 dolar"
             , "1.42 dolardan fazla"
             , "bir dolar 42 sentin üstünde"
             ]
  ]