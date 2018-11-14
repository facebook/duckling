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
  , examples (simple GBP 3.01)
             [ "GBP3.01"
             , "GBP 3.01"
             ]

  ]
