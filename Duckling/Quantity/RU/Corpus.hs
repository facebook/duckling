-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Quantity.RU.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Resolve
import Duckling.Quantity.Types
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale RU Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Pound 2 Nothing)
             [ "2 фунта"
             ]
  , examples (simple Gram 2 Nothing)
             [ "2 грамма"
             , "два грамма"
             , "0.002 кг"
             , "2000 миллиграмм"
             , "2000 мг"
             ]
  , examples (simple Gram 1000 Nothing)
             [ "килограмм"
             , "кг"
             ]
  , examples (simple Gram 2000 Nothing)
             [ "2 килограмма"
             , "2 кг"
             ]
  , examples (simple Pound 1 Nothing)
             [ "фунт"
             , "1 фунт"
             ]
  , examples (simple Ounce 2 Nothing)
             [ "2 унции"
             ]
  , examples (simple Gram 500 Nothing)
             [ "500 грамм"
             , "500г"
             , "500 г"
             , "0.5 кг"
             , "пятьсот грамм"
             ]
  ]
