-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.PL.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale PL Nothing}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumeralValue 0)
             [ "0"
             , "nic"
             , "zero"
             ]
  , examples (NumeralValue 1)
             [ "1"
             , "jeden"
             , "pojedynczy"
             ]
  , examples (NumeralValue 2)
             [ "2"
             , "dwa"
             , "para"
             ]
  , examples (NumeralValue 33)
             [ "33"
             , "trzydzieści trzy"
             , "0033"
             ]
  , examples (NumeralValue 14)
             [ "14"
             , "czternaście"
             ]
  , examples (NumeralValue 16)
             [ "16"
             , "szesnaście"
             ]
  , examples (NumeralValue 17)
             [ "17"
             , "siedemnaście"
             ]
  , examples (NumeralValue 18)
             [ "18"
             , "osiemnaście"
             ]
  , examples (NumeralValue 1.1)
             [ "1.1"
             , "1.10"
             , "01.10"
             ]
  , examples (NumeralValue 0.77)
             [ "0.77"
             , ".77"
             ]
  , examples (NumeralValue 100000)
             [ "100,000"
             , "100000"
             , "100K"
             , "100k"
             ]
  , examples (NumeralValue 3000000)
             [ "3M"
             , "3000K"
             , "3000000"
             , "3,000,000"
             ]
  , examples (NumeralValue 1200000)
             [ "1,200,000"
             , "1200000"
             , "1.2M"
             , "1200K"
             ]
  , examples (NumeralValue (-1200000))
             [ "- 1,200,000"
             , "-1200000"
             , "minus 1,200,000"
             , "-1.2M"
             , "-1200K"
             , "-.0012G"
             ]
  , examples (NumeralValue 5000)
             [ "5 tysięcy"
             , "pięć tysięcy"
             ]
  , examples (NumeralValue 122)
             [ "sto dwadzieścia dwa"
             ]
  , examples (NumeralValue 200000)
             [ "dwieście tysięcy"
             ]
  , examples (NumeralValue 21011)
             [ "dwadzieścia jeden tysięcy i jedenaście"
             , "dwadzieścia jeden tysięcy jedenaście"
             ]
  , examples (NumeralValue 721012)
             [ "siedemset dwadzieścia jeden tysięcy dwanaście"
             , "siedemset dwadzieścia jeden tysięcy i dwanaście"
             ]
  , examples (NumeralValue 65000000)
             [ "sześćdziesiąt pięć milionów"
             ]
  , examples (NumeralValue 31256721)
             [ "trzydzieści jeden milionów dwieście pięćdziesiąt sześć tysięcy siedemset dwadzieścia jeden"
             ]
  , examples (NumeralValue 15)
             [ "piętnasta"
             ]
  ]
