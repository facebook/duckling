-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Quantity.EN.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Quantity.Types
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Pound 2 (Just "meat"))
             [ "two pounds of meat"
             ]
  , examples (simple Gram 2 Nothing)
             [ "2 grams"
             , "0.002 kg"
             , "2/1000 kilograms"
             , "2000 milligrams"
             ]
  , examples (simple Gram 1000 Nothing)
             [ "a kilogram"
             , "a kg"
             ]
  , examples (simple Pound 1 Nothing)
             [ "a Pound"
             , "1 lb"
             , "a lb"
             ]
  , examples (simple Ounce 2 Nothing)
             [ "2 ounces"
             , "2oz"
             ]
  , examples (simple Cup 3 (Just "sugar"))
             [ "3 Cups of sugar"
             , "3 Cups of SugAr"
             ]
  , examples (simple Cup 0.75 Nothing)
             [ "3/4 cup"
             , "0.75 cup"
             , ".75 cups"
             ]
  , examples (simple Gram 500 (Just "strawberries"))
             [ "500 grams of strawberries"
             , "500g of strawberries"
             , "0.5 kilograms of strawberries"
             , "0.5 kg of strawberries"
             , "500000mg of strawberries"
             ]
  , examples (between Gram (100,1000) (Just "strawberries"))
              [ "100-1000 gram of strawberries"
              , "between 100 and 1000 grams of strawberries"
              , "from 100 to 1000 g of strawberries"
              , "100 - 1000 g of strawberries"
              ]
  , examples (between Gram (2,7) Nothing)
              [ "around 2 -7 g"
              , "~2-7 grams"
              , "from 2 to 7 g"
              , "between 2.0 g and about 7.0 g"
              , "between 0.002 kg and about 0.007 kg"
              , "2 - ~7 grams"
              ]
  , examples (under Pound 6 (Just "meat"))
              [ "less than six pounds of meat"
              , "no more than 6 lbs of meat"
              , "below 6.0 pounds of meat"
              , "at most six pounds of meat"
              ]
  , examples (above Cup 2 Nothing)
              [ "exceeding 2 Cups"
              , "at least two Cups"
              , "over 2 Cups"
              , "more than 2 Cups"
              ]
  , examples (above Ounce 4 (Just "chocolate"))
              [ "exceeding 4 oz of chocolate"
              , "at least 4.0 oz of chocolate"
              , "over four ounces of chocolate"
              , "more than four ounces of chocolate"
              ]
  ]
