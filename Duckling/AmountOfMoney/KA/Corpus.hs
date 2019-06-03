-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.KA.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.AmountOfMoney.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale KA Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Dollar 1)
             [ "$1"
             , "ერთი დოლარი"
             , "1 დოლარი"
             ]
  , examples (simple Dollar 10)
             [ "$10"
             , "$ 10"
             , "10$"
             , "10 დოლარი"
             , "ათი დოლარი"
             ]
  , examples (simple Cent 10)
             [ "10 ცენტი"
             , "ათი ცენტი"
             , "10 c"
             , "10¢"
             ]
  , examples (simple Dollar 1e4)
             [ "$10K"
             , "10k$"
             , "$10,000"
             ]
  , examples (simple USD 3.14)
             [ "USD3.14"
             , "3.14US$"
             , "US$ 3.14"
             ]
  , examples (simple EUR 20)
             [ "20\x20ac"
             , "20 ევრო"
             , "EUR 20"
             , "EUR 20.0"
             , "20€"
             , "20 €ur"
             ]
  , examples (simple Pound 10)
             [ "\x00a3\&10"
             , "ათი ფუნტი"
             ]
  , examples (simple Dollar 20.43)
             [ "$20 და 43c"
             , "20 დოლარი და 43c"
             , "20 დოლარი 43 ცენტი"
             , "ოცი დოლარი 43 ცენტი"
             , "ოცი დოლარი და ორმოცდასამი ცენტი"
             ]
  , examples (simple GBP 3.01)
             [ "GBP3.01"
             , "GBP 3.01"
             , "3 GBP 1 პენსი"
             ]
  , examples (simple KWD 42)
             [ "42 KWD"
             , "42 ქუვეითური დინარი"
             ]
  , examples (simple LBP 42)
             [ "42 LBP"
             , "42 ლიბანური ფუნტი"
             ]
  , examples (simple EGP 42)
             [ "42 EGP"
             , "42 ეგვიპტური ფუნტი"
             ]
  , examples (simple QAR 42)
             [ "42 QAR"
             , "42 კატარული რიალი"
             ]
  , examples (between Dollar (10, 20))
             [ "დაახლოებით 10-20 დოლარი"
             , "10 დოლარიდან 20 დოლარამდე"
             , "10-დან 20 დოლარამდე"
             , "გძეტა $10-$20"
             , "10-20 დოლარი"
             ]
  , examples (between Dollar (1.1, 1.3))
             [ "1.1-დან 1.3 დოლარამდე"
             , "ერთი მთელი ერთი დოლარიდან ერთი მთელი სამ დოლარამდე"
             ]
  , examples (under EUR 7)
             [ "7 ევრომდე"
             , "7 ევროზე ნაკლები"
             ]
  , examples (above Dollar 1.42)
             [ "1 დოლარი და 42 ცენტზე მეტი"
             ]
  , examples (between GEL (10, 20))
             [ "დაახლოებით 10-20 ლარი"
             , "10 ლარიდან 20 ლარამდე"
             , "10-დან 20 ლარამდე"
             , "გძეტა 10-20 ლარი"
             , "10-20 ლარი"
             ]
  , examples (between GEL (1.1, 1.3))
             [ "1.1-დან 1.3 ლარამდე"
             , "ერთი მთელი ერთი ლარიდან ერთი მთელი სამ ლარამდე"
             ]
  , examples (under GEL 7)
             [ "7 ლარამდე"
             , "7 ლარზე ნაკლები"
             ]
  , examples (above GEL 1.42)
             [ "1 ლარი და 42 თეთრზე მეტი"
             ]
  , examples (between GEL (1, 4))
             [ "1ლარიდან 4ლარამდე"
             ]
  ]
