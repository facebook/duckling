-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Volume.TR.Corpus
  ( corpus ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types
import Duckling.Volume.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale TR Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Millilitre 250)
             [ "250 mililitre"
             , "250ml"
             , "250 ml"
             ]
  , examples (simple Litre 2)
             [ "2 litre" ]
  , examples (simple Gallon 3)
             [ "3 galon"
             , "3 gal"
             ]
  , examples (simple Hectolitre 3)
             [ "3 hektolitre"
             ]
  , examples (simple Litre 0.5)
             [ "yarım litre"
             ]
  ]
