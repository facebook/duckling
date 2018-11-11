-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.MN.Corpus
  ( corpus 
  ) where

import Data.String
import Prelude

import Duckling.AmountOfMoney.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale MN Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple MNT 1)
             [ "1 төг"
             , "нэг төгрөг"
             , "1 ₮"
             , "1 төгрөг"
             ]
  , examples (simple MNT 10)
             [ "10 төгрөг"
             , "₮ 10"
             , "10₮"
             , "10MNT"
             , "10төг"
             , "10 төгрөг"
             , "төгрөг 10"
             ]
  , examples (simple Dollar 1)
             [ "$1"
             , "нэг доллар"
             ]
  , examples (simple Dollar 10)
             [ "$10"
             , "$ 10"
             , "10$"
             , "10 доллар"
             , "арван доллар"
             ]
  , examples (simple Cent 10)
             [ "10 цент"
             , "арван пени"
             , "арван цент"
             , "10 c"
             , "10¢"
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
             [ "20€"
             , "20 €ur"
             , "20 евро"
             , "Евро 20"
             ]
  , examples (simple Pound 10)
             [ "\x00a3\&10"
             , "арван фунт"
             ]
  , examples (simple INR 20)
             [ "20Rs"
             , "Rs20"
             ]
  , examples (simple Dollar 20.43)
             [ "$20 43"
             , "20 доллар 43ц"
             , "20 доллар 43 центийн"
             , "20 доллар 43 центээр"
             , "20 доллар 43"
             , "хорин доллар 43"
             ]
  , examples (simple GBP 3.01)
             [ "GBP3.01"
             , "GBP 3.01"
             , "3 GBP 1 пенс"
             ]
  , examples (simple Unnamed 42)
             [ "42 бакс"
             , "бакс 42"
             , "42 бакс"
             ]
  , examples (simple BYN 42)
             [ "42 BYN"
             ]
  , examples (simple KWD 42)
             [ "42 KWD"
             ]
  , examples (simple LBP 42)
             [ "42 LBP"
             ]
  , examples (simple EGP 42)
             [ "42 EGP"
             ]
  , examples (simple QAR 42)
             [ "42 QAR"
             ]
  , examples (simple SAR 42)
             [ "42 SAR"
             ]
  , examples (simple BGN 42)
             [ "42 BGN"
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
             ]
  , examples (between Dollar (10, 20))
             [ "10 болон 20 долларын хооронд"
             , "10 аас 20 доллар хүртэл"
             , "10-20 доллар"
             , "10$-20$"
             , "10-20 доллар"
             ]
  , examples (under EUR 7)
             [ "7 еврогоос бага"
             , "7 EUR-с бага"
             , "7€ бага"
             , "7 еврогоос ихгүй"
             , "7 евро хүрэхгүй"
             ]
  
  ]
