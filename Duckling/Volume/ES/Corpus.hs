-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Volume.ES.Corpus
  ( corpus ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Resolve
import Duckling.Volume.Types
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale ES Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Millilitre 250)
             [ "250 mililitros"
             , "250ml"
             , "250 ml"
             ]
  , examples (simple Litre 2)
             [ "2 litros"
             ]
  , examples (simple Gallon 3)
             [ "3 galón"
             ]
  , examples (simple Hectolitre 3)
             [ "3 hectolitros"
             ]
  , examples (simple Litre 0.5)
             [ "medio litro"
             ]
  ]
