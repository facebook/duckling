-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Volume.EN.Corpus
  ( corpus ) where

import Data.String
import Prelude

import Duckling.Testing.Types
import Duckling.Volume.Types

corpus :: Corpus
corpus = (testContext, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Litre 1)
             [ "1 liter"
             , "1 litre"
             , "one liter"
             , "a liter"
             ]
  , examples (simple Litre 2)
             [ "2 liters"
             , "2l"
             ]
  , examples (simple Litre 1000)
             [ "1000 liters"
             , "thousand liters"
             ]
  , examples (simple Litre 0.5)
             [ "half liter"
             , "half-litre"
             , "half a liter"
             ]
  , examples (simple Litre 0.25)
             [ "quarter-litre"
             , "fourth of liter"
             ]
  , examples (simple Millilitre 1)
             [ "one milliliter"
             , "an ml"
             , "a millilitre"
             ]
  , examples (simple Millilitre 250)
             [ "250 milliliters"
             , "250 millilitres"
             , "250ml"
             , "250mls"
             , "250 ml"
             ]
  , examples (simple Gallon 3)
             [ "3 gallons"
             , "3 gal"
             , "3gal"
             , "around three gallons"
             ]
  , examples (simple Gallon 0.5)
             [ "0.5 gals"
             , "1/2 gallon"
             , "half a gallon"
             ]
  , examples (simple Gallon 0.1)
             [ "0.1 gallons"
             , "tenth of a gallon"
             ]
  , examples (simple Hectolitre 3)
             [ "3 hectoliters"
             ]
  , examples (between Litre (100,1000))
             [ "between 100 and 1000 liters"
             , "100-1000 liters"
             , "from 100 to 1000 l"
             , "100 - 1000 l"
             ]
  , examples (between Litre (2,7))
             [ "around 2 -7 l"
             , "~2-7 liters"
             , "from 2 to 7 l"
             , "between 2.0 l and about 7.0 l"
             , "between 2l and about 7l"
             , "2 - ~7 litres"
             ]
  , examples (under Gallon 6)
             [ "less than six gallons"
             , "under six gallon"
             , "no more than 6 gals"
             , "below 6.0gal"
             , "at most six gallons"
             ]
  , examples (above Hectolitre 2)
             [ "exceeding 2 hectoliters"
             , "at least two hectolitres"
             , "over 2 hectolitre"
             , "more than 2 hectoliter"
             ]
  , examples (above Millilitre 4)
             [ "exceeding 4 ml"
             , "at least 4.0 ml"
             , "over four milliliters"
             , "more than four mls"
             ]
  ]
