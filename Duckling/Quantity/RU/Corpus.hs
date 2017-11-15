-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


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
corpus = (testContext {locale = makeLocale RU Nothing}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (QuantityData Pound 2 Nothing)
             [ "2 фунта"
             ]
  , examples (QuantityData Gram 2 Nothing)
             [ "2 грамма"
             , "два грамма"
             , "0.002 кг"
             , "2000 миллиграмм"
             , "2000 мг"
             ]
  , examples (QuantityData Gram 1000 Nothing)
             [ "килограмм"
             , "кг"
             ]
  , examples (QuantityData Gram 2000 Nothing)
             [ "2 килограмма"
             , "2 кг"
             ]
  , examples (QuantityData Pound 1 Nothing)
             [ "фунт"
             , "1 фунт"
             ]
  , examples (QuantityData Ounce 2 Nothing)
             [ "2 унции"
             ]
  , examples (QuantityData Gram 500 Nothing)
             [ "500 грамм"
             , "500г"
             , "500 г"
             , "0.5 кг"
             , "пятьсот грамм"
             ]
  ]
