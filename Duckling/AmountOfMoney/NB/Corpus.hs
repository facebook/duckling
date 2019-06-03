-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


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
  , examples (simple SEK 2.23)
             [ "2 kronor og 23 öre"
             , "to kronor 23 öre"
             , "to svenske kronor 23 öre"
             , "to svenske kroner 23 öre"
             , "to svenske kroner 23 øre"
             , "to kronor og 23 öre"
             , "to svenske kronor og 23 öre"
             , "to svenske kroner og 23 öre"
             , "to svenske kroner og 23 øre"
             , "to svensk kroner og 23 øre"
             ]
  , examples (simple DKK 2.23)
             [ "2 danske kroner og 23 øre"
             , "to danske kroner 23 øre"
             , "to danske kroner og 23 øre"
             , "to danske kr 23"
             , "to dansk kr 23"
             ]
  , examples (simple USD 2.23)
             [ "2 amerikanske dollar og 23 cent"
             , "2 amerikanske dollars og 23 cent"
             , "2 amerikanske dollar og 23 cents"
             , "2 amerikanske dollars og 23 cents"
             , "to amerikanske dollar 23 cent"
             , "to amerikanske dollars 23 cent"
             , "to amerikanske dollar 23 cents"
             , "to amerikanske dollars 23 cents"
             , "to amerikanske dollar og 23 cent"
             , "to amerikanske dollars og 23 cent"
             , "to amerikanske dollar og 23 cents"
             , "to amerikanske dollars og 23 cents"
             , "to amerikanske dollar 23"
             , "to amerikanske dollars 23"
             ]
  , examples (simple AUD 2.23)
             [ "2 australske dollar og 23 cent"
             , "2 australske dollars og 23 cent"
             , "2 australske dollar og 23 cents"
             , "2 australske dollars og 23 cents"
             , "to australske dollar 23 cent"
             , "to australske dollars 23 cent"
             , "to australske dollar 23 cents"
             , "to australske dollars 23 cents"
             , "to australske dollar og 23 cent"
             , "to australske dollars og 23 cent"
             , "to australske dollar og 23 cents"
             , "to australske dollars og 23 cents"
             , "to australske dollar 23"
             , "to australske dollars 23"
             ]
  , examples (simple CAD 2.23)
             [ "2 kanadiske dollar og 23 cent"
             , "2 kanadiske dollars og 23 cent"
             , "2 canadiske dollar og 23 cent"
             , "2 canadiske dollars og 23 cent"
             , "2 kanadiske dollar og 23 cents"
             , "2 kanadiske dollars og 23 cents"
             , "2 canadiske dollar og 23 cents"
             , "2 canadiske dollars og 23 cents"
             , "to kanadiske dollar 23 cent"
             , "to kanadiske dollars 23 cent"
             , "to canadiske dollar 23 cent"
             , "to canadiske dollars 23 cent"
             , "to kanadiske dollar 23 cents"
             , "to kanadiske dollars 23 cents"
             , "to canadiske dollar 23 cents"
             , "to canadiske dollars 23 cents"
             , "to kanadiske dollar og 23 cent"
             , "to kanadiske dollars og 23 cent"
             , "to canadiske dollar og 23 cent"
             , "to canadiske dollars og 23 cent"
             , "to kanadiske dollar og 23 cents"
             , "to kanadiske dollars og 23 cents"
             , "to canadiske dollar og 23 cents"
             , "to canadiske dollars og 23 cents"
             , "to kanadiske dollar 23"
             , "to kanadiske dollars 23"
             , "to canadiske dollar 23"
             , "to canadiske dollars 23"
             ]
  , examples (simple CHF 2.23)
             [ "2 sveitsiske franc og 23 rappen"
             , "2 sveitsiske francs og 23 rappen"
             , "2 sveitsiske franc og 23 rp"
             , "2 sveitsiske francs og 23 rp"
             , "2 sveitsiske franc og 23 centime"
             , "2 sveitsiske francs og 23 centime"
             , "2 sveitsiske franc og 23 c"
             , "2 sveitsiske francs og 23 c"
             , "2 sveitsiske franc og 23 centesimo"
             , "2 sveitsiske francs og 23 centesimo"
             , "2 sveitsiske franc og 23 ct"
             , "2 sveitsiske francs og 23 ct"
             , "2 sveitsiske franc og 23 rap"
             , "2 sveitsiske francs og 23 rap"
             , "to sveitsiske franc 23 rappen"
             , "to sveitsiske francs 23 rappen"
             , "to sveitsiske franc 23 rp"
             , "to sveitsiske francs 23 rp"
             , "to sveitsiske franc 23 centime"
             , "to sveitsiske francs 23 centime"
             , "to sveitsiske franc 23 c"
             , "to sveitsiske francs 23 c"
             , "to sveitsiske franc 23 centesimo"
             , "to sveitsiske francs 23 centesimo"
             , "to sveitsiske franc 23 ct"
             , "to sveitsiske francs 23 ct"
             , "to sveitsiske franc 23 rap"
             , "to sveitsiske francs 23 rap"
             , "to sveitsiske franc og 23 rappen"
             , "to sveitsiske francs og 23 rappen"
             , "to sveitsiske franc og 23 rp"
             , "to sveitsiske francs og 23 rp"
             , "to sveitsiske franc og 23 centime"
             , "to sveitsiske francs og 23 centime"
             , "to sveitsiske franc og 23 c"
             , "to sveitsiske francs og 23 c"
             , "to sveitsiske franc og 23 centesimo"
             , "to sveitsiske francs og 23 centesimo"
             , "to sveitsiske franc og 23 ct"
             , "to sveitsiske francs og 23 ct"
             , "to sveitsiske franc og 23 rap"
             , "to sveitsiske francs og 23 rap"
             , "to sveitsiske franc 23"
             , "to sveitsiske francs 23"
             ]
  , examples (simple CNY 2.23)
             [ "2 yuan og 23 fen"
             , "2 kinesiske yuan og 23 fen"
             , "2 renminbi og 23 fen"
             , "2 kinesiske renminbi og 23 fen"
             , "to yuan 23 fen"
             , "to kinesiske yuan 23 fen"
             , "to renminbi 23 fen"
             , "to kinesiske renminbi 23 fen"
             , "to yuan og 23 fen"
             , "to kinesiske yuan og 23 fen"
             , "to renminbi og 23 fen"
             , "to kinesiske renminbi og 23 fen"
             , "to yuan 23"
             , "to kinesiske yuan 23"
             , "to renminbi 23"
             , "to kinesiske renminbi 23"
             ]
  , examples (simple CZK 2.23)
             [ "2 koruna og 23 haleru"
             , "2 tsjekkiske koruna og 23 haleru"
             , "to koruna 23 haleru"
             , "to tsjekkiske koruna 23 haleru"
             , "to koruna og 23 haleru"
             , "to tsjekkiske koruna og 23 haleru"
             , "to koruna 23"
             , "to tsjekkiske koruna 23"
             ]
  , examples (simple HKD 2.23)
             [ "2 hong kong dollar og 23 cent"
             , "2 hong kong dollar og 23 cents"
             , "2 hong kong dollars og 23 cent"
             , "2 hong kong dollars og 23 cents"
             , "2 hong kong dollar og 23 cents"
             , "2 hong kong dollars og 23 cents"
             , "to hong kong dollar 23 cent"
             , "to hong kong dollars 23 cent"
             , "to hong kong dollar 23 cents"
             , "to hong kong dollars 23 cents"
             , "to hong kong dollar og 23 cent"
             , "to hong kong dollars og 23 cent"
             , "to hong kong dollar og 23 cents"
             , "to hong kong dollars og 23 cents"
             , "to hong kong dollar 23"
             , "to hong kong dollars 23"
             ]
  , examples (simple INR 2.23)
             [ "2 indiske rupi og 23 paise"
             , "2 indiske rupier og 23 paise"
             , "to indiske rupi 23 paise"
             , "to indiske rupier 23 paise"
             , "to indiske rupier 23 paise"
             , "to indiske rupi og 23 paise"
             , "to indiske rupier og 23 paise"
             , "to indiske rupi 23"
             , "to indiske rupier 23"
             ]
  , examples (simple NZD 2.23)
             [ "2 new zealand dollar og 23 cent"
             , "2 new zealand dollars og 23 cent"
             , "2 new zealand dollars og 23 cents"
             , "2 new zealandske dollar og 23 cent"
             , "2 new zealandske dollars og 23 cent"
             , "2 new zealandske dollars og 23 cents"
             , "2 new zealand dollar og 23 cents"
             , "2 new zealand dollars og 23 cents"
             , "2 nz dollar og 23 cents"
             , "2 nz dollars og 23 cents"
             , "to new zealand dollar 23 cent"
             , "to new zealand dollars 23 cent"
             , "to new zealand dollars 23 cents"
             , "to new zealandske dollar 23 cent"
             , "to new zealandske dollars 23 cent"
             , "to new zealandske dollars 23 cents"
             , "to new zealand dollar 23 cents"
             , "to new zealand dollars 23 cents"
             , "to new zealand dollars 23 cent"
             , "to new zealand dollar og 23 cent"
             , "to new zealand dollars og 23 cent"
             , "to new zealandske dollar og 23 cent"
             , "to new zealandske dollars og 23 cent"
             , "to new zealandske dollars og 23 cents"
             , "to new zealand dollar og 23 cents"
             , "to new zealand dollars og 23 cent"
             , "to new zealand dollars og 23 cents"
             , "to new zealand dollar 23"
             , "to new zealand dollars 23"
             , "to new zealandske dollar 23"
             , "to new zealandske dollars 23"
             , "to nz dollar 23"
             , "to nz dollars 23"
             ]
  , examples (simple PLN 2.23)
             [ "2 zloty og 23 groszy"
             , "2 sloty og 23 groszy"
             , "2 polske zloty og 23 groszy"
             , "2 polske sloty og 23 groszy"
             , "to zloty 23 groszy"
             , "to sloty 23 groszy"
             , "to polske zloty 23 groszy"
             , "to polske sloty 23 groszy"
             , "to zloty og 23 groszy"
             , "to sloty og 23 groszy"
             , "to polske zloty og 23 groszy"
             , "to polske sloty og 23 groszy"
             , "to zloty 23"
             , "to sloty 23"
             , "to polske zloty 23"
             , "to polske sloty 23"
             ]
  , examples (simple SGD 2.23)
             [ "2 singapore dollar og 23 cent"
             , "2 singapore dollars og 23 cent"
             , "2 singaporske dollar og 23 cent"
             , "2 singaporske dollars og 23 cent"
             , "2 singapore dollar og 23 cents"
             , "2 singapore dollars og 23 cents"
             , "to singapore dollar 23 cent"
             , "to singapore dollars 23 cent"
             , "to singaporske dollar 23 cent"
             , "to singaporske dollars 23 cent"
             , "to singapore dollar 23 cents"
             , "to singapore dollars 23 cents"
             , "to singapore dollar og 23 cent"
             , "to singapore dollars og 23 cent"
             , "to singaporske dollar og 23 cent"
             , "to singaporske dollars og 23 cent"
             , "to singapore dollar og 23 cents"
             , "to singapore dollars og 23 cents"
             , "to singapore dollar 23"
             , "to singapore dollars 23"
             , "to singaporske dollar 23"
             , "to singaporske dollars 23"
             ]
  , examples (simple ZAR 2.23)
             [ "2 rand og 23 cent"
             , "2 sørafrikanske rand og 23 cent"
             , "2 sørafrikanske rand og 23 cents"
             , "2 sør-afrikanske rand og 23 cent"
             , "2 sør-afrikanske rand og 23 cents"
             , "to rand 23 cent"
             , "to sørafrikanske rand 23 cent"
             , "to sørafrikanske rand 23 cents"
             , "to sør-afrikanske rand 23 cent"
             , "to sør-afrikanske rand 23 cents"
             , "to rand og 23 cent"
             , "to sørafrikanske rand og 23 cent"
             , "to sørafrikanske rand og 23 cents"
             , "to sør-afrikanske rand og 23 cent"
             , "to sør-afrikanske rand og 23 cents"
             , "to rand 23"
             , "to sørafrikanske rand 23"
             , "to sør-afrikanske rand 23"
             ]
  , examples (simple THB 2.23)
             [ "2 baht og 23 satang"
             , "2 bhat og 23 satang"
             , "2 thai baht og 23 satang"
             , "2 thai bhat og 23 satang"
             , "2 thailandske baht og 23 satang"
             , "2 thailandske bhat og 23 satang"
             , "to baht 23 satang"
             , "to bhat 23 satang"
             , "to thai baht 23 satang"
             , "to thai bhat 23 satang"
             , "to thailandske baht 23 satang"
             , "to thailandske bhat 23 satang"
             , "to baht og 23 satang"
             , "to bhat og 23 satang"
             , "to thai baht og 23 satang"
             , "to thai bhat og 23 satang"
             , "to thailandske baht og 23 satang"
             , "to thailandske bhat og 23 satang"
             , "to baht 23"
             , "to bhat 23"
             , "to thai baht 23"
             , "to thai bhat 23"
             , "to thailandske baht 23"
             , "to thailandske bhat 23"
             ]
  , examples (simple Cent 10)
             [ "ti cent"
             , "ti cents"
             , "ti pence"
             , "ti penny"
             , "ti pennies"
             , "10 cent"
             , "10 cents"
             , "10 cents"
             , "10 pence"
             , "10 penny"
             , "10 øre"
             , "10 ører"
             , "10 öre"
             , "10 örer"
             , "10 p"
             , "10 c"
             , "10 fen"
             , "10 haleru"
             , "10 groszy"
             , "10 paise"
             , "10 paisa"
             , "10 centesimo"
             , "10 centesimi"
             , "10 centime"
             , "10 centimes"
             , "10 ct"
             , "10 rap"
             , "10 rappen"
             , "10 rappens"
             , "10 rp"
             , "10 satang"
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
