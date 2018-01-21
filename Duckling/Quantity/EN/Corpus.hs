-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Quantity.EN.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Quantity.Types
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (QuantityData Pound 2 (Just "meat"))
             [ "two pounds of meat"
             ]
  , examples (QuantityData Gram 2 Nothing)
             [ "2 grams"
             , "0.002 kg"
             , "2/1000 kilograms"
             , "2000 milligrams"
             ]
  , examples (QuantityData Gram 1000 Nothing)
             [ "a kilogram"
             , "a kg"
             ]
  , examples (QuantityData Pound 1 Nothing)
             [ "a Pound"
             , "1 lb"
             , "a lb"
             ]
  , examples (QuantityData Ounce 2 Nothing)
             [ "2 ounces"
             , "2oz"
             ]
  , examples (QuantityData Cup 3 (Just "sugar"))
             [ "3 Cups of sugar"
             ]
  , examples (QuantityData Cup 0.75 Nothing)
             [ "3/4 cup"
             , "0.75 cup"
             , ".75 cups"
             ]
  , examples (QuantityData Gram 500 (Just "strawberries"))
             [ "500 grams of strawberries"
             , "500g of strawberries"
             , "0.5 kilograms of strawberries"
             , "0.5 kg of strawberries"
             , "500000mg of strawberries"
             ]
  ]
