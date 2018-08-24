-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.NB.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.AmountOfMoney.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale NB Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Dollar 10)
             [ "$10"
             , "10$"
             , "ti dollar"
             ]
  , examples (simple Cent 10)
             [ "ti øre"
             ]
  , examples (simple Dollar 10000)
             [ "$10.000"
             , "10K$"
             , "$10k"
             ]
  , examples (simple USD 1.23)
             [ "USD1,23"
             ]
  , examples (simple NOK 10)
             [ "10kroner"
             , "10kr"
             , "ti kroner"
             , "10 NOK"
             , "ti norske kroner"
             ]
  , examples (simple SEK 10)
             [ "10kronor"
             , "10 svenske kroner"
             , "10 svenske kr"
             , "10 svenske kronor"
             , "ti kronor"
             ]
  , examples (simple DKK 10)
             [ "10 danske kroner"
             , "ti danske kroner"
             , "ti danske kr"
             ]
  , examples (simple AUD 10)
             [ "10 australske dollar"
             , "10 australske dollars"
             ]
  , examples (simple CAD 10)
             [ "10 kanadiske dollar"
             , "10 kanadiske dollars"
             , "10 canadiske dollar"
             , "10 canadiske dollars"
             ]
  , examples (simple CHF 10)
             [ "10 sveitsiske franc"
             , "10 sveitsiske francs"
             ]
  , examples (simple CNY 10)
             [ "10 yuan"
             , "10 kinesiske yuan"
             , "10 kinesisk yuan"
             , "10 renminbi"
             , "10 kinesiske renminbi"
             , "10 kinesisk renminbi"
             ]
  , examples (simple CZK 10)
             [ "10 koruna"
             , "10 tsjekkiske koruna"
             ]
  , examples (simple INR 10)
             [ "10 rupi"
             , "10 rupier"
             , "10 rupee"
             , "10 rupees"
             , "10 indisk rupi"
             , "10 indiske rupi"
             , "10 indiske rupee"
             , "10 indiske rupees"
             , "10 indiske rupier"
             , "10 indisk rupier"
             , "ti rupi"
             , "ti rupier"
             , "ti indisk rupi"
             , "ti indiske rupi"
             , "ti indiske rupee"
             , "ti indiske rupees"
             , "ti indiske rupier"
             , "ti indisk rupier"
             ]
  , examples (simple JPY 10)
             [ "10 japanske yen"
             , "10 yen"
             , "ti japanske yen"
             , "ti yen"
             ]
  , examples (simple PKR 10)
             [ "10 pakistansk rupi"
             , "10 pakistanske rupi"
             , "10 pakistanske rupee"
             , "10 pakistanske rupees"
             , "10 pakistanske rupier"
             , "10 pakistansk rupier"
             , "ti pakistansk rupi"
             , "ti pakistanske rupi"
             , "ti pakistanske rupee"
             , "ti pakistanske rupees"
             , "ti pakistanske rupier"
             , "ti pakistansk rupier"
             ]
  , examples (simple PLN 10)
             [ "10 zloty"
             , "10 sloty"
             , "10 polske zloty"
             , "10 polske sloty"
             , "ti zloty"
             , "ti sloty"
             , "ti polske zloty"
             , "ti polske sloty"
             ]
  , examples (simple SGD 10)
             [ "10 singapore dollar"
             , "10 singapore dollars"
             , "10 singaporske dollar"
             , "10 singaporske dollars"
             , "ti singapore dollar"
             , "ti singapore dollars"
             , "ti singaporske dollar"
             , "ti singaporske dollars"
             ]
  , examples (simple THB 10)
             [ "10 baht"
             , "10 bhat"
             , "10 thai baht"
             , "10 thai bhat"
             , "10 thailand baht"
             , "10 thailand bhat"
             , "10 thailandske baht"
             , "10 thailandske bhat"
             , "ti baht"
             , "ti bhat"
             , "ti thai baht"
             , "ti thai bhat"
             , "ti thailand baht"
             , "ti thailand bhat"
             , "ti thailandske baht"
             , "ti thailandske bhat"
             ]
  , examples (simple ZAR 10)
             [ "10 rand"
             , "10 sørafrikanske rand"
             , "10 sør-afrikanske rand"
             , "10 rand"
             , "10 sørafrikanske rand"
             , "10 sør-afrikanske rand"
             , "ti rand"
             , "ti sørafrikanske rand"
             , "ti sør-afrikanske rand"
             ]
  , examples (simple NOK 2.23)
             [ "2 kroner og 23 øre"
             , "to kroner 23 øre"
             , "to kroner og 23 øre"
             , "to kr 23"
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
             , "INR20"
             ]
  , examples (simple INR 20.43)
             [ "20 Rupees 43"
             , "tjue rupees 43"
             , "tjue rupees 43¢"
             ]
  , examples (simple Pound 9)
             [ "£9"
             , "ni pund"
             ]
  , examples (simple GBP 3.01)
             [ "GBP3,01"
             , "GBP 3,01"
             ]
  ]
