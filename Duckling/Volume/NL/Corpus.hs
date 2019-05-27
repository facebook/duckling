-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Volume.NL.Corpus
  ( corpus ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Resolve
import Duckling.Volume.Types
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale NL Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Millilitre 250)
             [ "250 milliliter"
             , "250ml"
             , "250 ml"
             ]
  , examples (simple Litre 2)
             [ "2 liter"
             , "2 liters"
             , "2l"
             , "2 l"
             ]
  , examples (simple Hectolitre 3)
             [ "3 hectoliter"
             , "3 hl"
             ]
  , examples (simple Litre 0.5)
             [ "halve liter"
             ]
  ]
