-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.

{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.HR.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.AmountOfMoney.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale HR Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Dollar 1)
             [ "$1"
             , "jedan dolar"
             ]
  , examples (simple Dollar 10)
             [ "$10"
             , "10$"
             , "deset dolara"
             ]
  , examples (simple Cent 1)
             [ "jedan cent"
             ]
  , examples (simple Cent 10)
             [ "deset centa"
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
             [ "2 dolara i 23 centa"
             , "dva dolara 23 centa"
             , "2 dolara 23"
             , "dva dolara i 23"
             ]
  , examples (simple HRK 1)
             [ "jedna kuna"
             ]
  , examples (simple HRK 2.23)
             [ "2 kune i 23 lipe"
             , "dvije kune 23 lipe"
             , "2 kune 23"
             , "dvije kune i 23"
             ]
  , examples (simple HRK 100)
             [ "100 kuna"
             , "sto kuna"
             ]
  , examples (simple HRK 200)
             [ "200 kuna"
             , "dvije stotine kuna"
             , "dvjesto kuna"
             , "dvjesta kuna"
             ]
  , examples (simple EUR 1)
             [ "jedan euro"
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
             , "20 Rupija"
             , "20Rs"
             , "Rs20"
             ]
  , examples (simple INR 20.43)
             [ "20 Rupija 43"
             , "dvadeset rupija 43"
             ]
  , examples (simple INR 33)
             [ "INR33"
             ]
  , examples (simple Pound 9)
             [ "£9"
             , "devet funti"
             ]
  , examples (simple GBP 3.01)
             [ "GBP3,01"
             , "GBP 3,01"
             ]
 , examples (between Dollar (100, 200))
            [ "od 100 do 200 dolara"
            , "od 100 dolara do 200 dolara"
            , "otprilike 100 do 200 dolara"
            , "otprilike 100 dolara do 200 dolara"
            , "100 dolara - 200 dolara"
            , "100-200 dolara"
            , "oko 100-200 dolara"
            , "u blizini 100-200 dolara"
            , "skoro 100-200 dolara"
            , "približno 100 do 200 dolara"
            , "izmedju 100 i 200 dolara"
            , "izmedju 100 dolara i 200 dolara"
            ]
  , examples (under EUR 7)
             [ "manje od EUR 7"
             , "ispod EUR 7"
             ]
  , examples (above Dollar 1.42)
             [ "više od jednog dolara i četrdeset dva centa"
             , "najmanje 1,42 dolara"
             , "preko 1,42 dolara"
             , "iznad 1,42 dolara"
             ]
 ]
