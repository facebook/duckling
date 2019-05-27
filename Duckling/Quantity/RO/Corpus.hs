-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Quantity.RO.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Quantity.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale RO Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Pound 2 (Just "carne"))
             [ "doua livre de carne"
             ]
  , examples (simple Pound 1 Nothing)
             [ "o livră"
             ]
  , examples (simple Pound 500 (Just "zahăr"))
             [ "cinci sute livre de zahăr"
             , "cinci sute de livre de zahăr"
             ]
  , examples (simple Pound 21 (Just "mamaliga"))
             [ "douăzeci și unu de livre de mamaliga"
             ]
  ]
