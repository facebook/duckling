-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.

{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.TR.Corpus
  ( allExamples
  , negativeExamples
  ) where

import Data.String
import Data.Text (Text)
import Prelude

import Duckling.AmountOfMoney.Types
import Duckling.Testing.Types

allExamples :: [Example]
allExamples = concat
  [ examples (simple TRY 10000)
             [ "10 bin lira"
             , "on bin lira"
             , "10000 lira"
             ]
  , examples (simple TRY 1)
             [ "lira"
             , "bir lira"
             , "bi lira"
             , "1 lira"
             ]
  , examples (simple TRY 0.1)
             [ "on kuruş"
             , "10 kuruş"
             ]
  , examples (simple TRY 0.25)
             [ "25 kuruş"
             , "yirmi beş kuruş"
             ]
  , examples (simple TRY 0.05)
             [ "beş kuruş"
             , "5 kuruş"
             ]
  ]

negativeExamples :: [Text]
negativeExamples =
  [ "kuruş lira",
    "300 kuruş 10 lira"
  ]
