-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.

{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.BG.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.AmountOfMoney.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale BG Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple BGN 1)
             [ "1 лв"
             , "един лев"
             , "1 Лев"
             ]
  , examples (simple BGN 10)
             [ "10 лв"
             , "десет лева"
             , "10лв"
             ]
  , examples (simple BGN 15.50)
             [ "15лв и 50ст"
             , "петнадесет лева и петдесет стотинки"
             , "15 Лв и 50 Ст"
             ]
  , examples (simple Dollar 1)
             [ "$1"
             , "един долар"
             , "1 долар"
             ]
  , examples (simple Dollar 10)
             [ "$10"
             , "$ 10"
             , "10$"
             , "10 Долара"
             , "десет долара"
             ]
  , examples (simple Cent 10)
             [ "10 цента"
             , "десет пенита"
             , "десет цента"
             , "10¢"
             ]
  , examples (simple Cent 50)
             [ "50 ст"
             , "петдесет стотинки"
             , "50ст"
             ]
  , examples (simple Dollar 1e4)
             [ "$10К"
             , "10к$"
             , "$10,000"
             ]
  , examples (simple USD 3.14)
             [ "USD3.14"
             , "3.14US$"
             , "US$ 3.14"
             ]
  , examples (simple EUR 20)
             [ "20\x20ac"
             , "20 евро"
             , "20 Евро"
             , "EUR 20"
             , "EUR 20.0"
             , "20€"
             , "20 €ur"
             ]
  , examples (simple Pound 10)
             [ "\x00a3\&10"
             , "десет паунда"
             ]
  , examples (simple Dollar 20.43)
             [ "$20 и 43ц"
             , "$20 43"
             , "20 долара 43ц"
             , "20 долара 43 цента"
             , "двадесет долара 43 цента"
             , "20 долара 43"
             , "двадесет долара и 43"
             ]
  , examples (simple GBP 3.01)
             [ "GBP3.01"
             , "GBP 3.01"
             , "3 GBP 1 пени"
             ]
  , examples (between Dollar (10, 20))
             [ "между 10 и 20 долара"
             , "от 10 до 20 долара"
             , "около 10-20 долара"
             , "между 10 и 20 долара"
             , "около $10-$20"
             , "10-20 долара"
             ]
  , examples (under EUR 7)
             [ "под седем евро"
             , "по-малко от 7 Евро"
             , "под 7€"
             ]
  , examples (above Dollar 1.42)
             [ "над 1 долар и четиридесет и два цента"
             , "поне $1.42"
             , "над 1.42 долара"
             ]
  ]
