-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Volume.GA.Corpus
  ( corpus ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Resolve
import Duckling.Volume.Types
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale GA Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Millilitre 250)
             [ "250 millilítir"
             , "250 millilitir"
             , "250ml"
             , "250 ml"
             ]
  , examples (simple Litre 2)
             [ "2 lítir"
             ]
  , examples (simple Gallon 5)
             [ "5 galúin"
             ]
  , examples (simple Hectolitre 20)
             [ "20 heictilitear"
             , "2 kl"
             ]
  ]
