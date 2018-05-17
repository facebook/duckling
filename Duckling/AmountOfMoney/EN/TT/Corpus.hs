-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.

{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.EN.TT.Corpus
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
  [ examples (simple TTD 1000)
             [ "a grand"
             , "1 grand"
             ]
  , examples (simple TTD 10000)
             [ "10 grand"
             ]
  ]

negativeExamples :: [Text]
negativeExamples =
  [ "grand"
  ]
