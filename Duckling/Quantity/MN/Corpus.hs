-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Quantity.MN.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Resolve
import Duckling.Quantity.Types
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale MN Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Pound 2 Nothing)
             [ "2 фунт"
             ]
  , examples (simple Gram 2 Nothing)
             [ "2 грамм"
             , "хоёр грамм"
             , "2000 миллиграмм"
             , "2000 мг"
             ]
  , examples (simple Gram 1000 Nothing)
             [ "килограмм"
             , "кг"
             ]
  , examples (simple Gram 2000 Nothing)
             [ "2 килограмм"
             , "2 кг"
             , "2000 грамм"
             ]
  , examples (simple Pound 1 Nothing)
             [ "фунт"
             , "1 фунт"
             ]
  , examples (simple Ounce 2 Nothing)
             [ "2 унц"
             ]
  , examples (simple Gram 500 Nothing)
             [ "500 грамм"
             , "500г"
             , "500 г"
             , "0.5 кг"
             ]
  ]
