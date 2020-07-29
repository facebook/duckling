-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


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
corpus = (testContext {locale = makeLocale PL Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple 0)
             [ "0"
             , "nic"
             , "zero"
             ]
  , examples (simple 1)
             [ "1"
             , "jeden"
             , "pojedynczy"
             ]
  , examples (simple 2)
             [ "2"
             , "dwa"
             , "para"
             ]
  , examples (simple 33)
             [ "33"
             , "trzydzieści trzy"
             , "0033"
             ]
  , examples (simple 14)
             [ "14"
             , "czternaście"
             ]
  , examples (simple 16)
             [ "16"
             , "szesnaście"
             ]
  , examples (simple 17)
             [ "17"
             , "siedemnaście"
             ]
  , examples (simple 18)
             [ "18"
             , "osiemnaście"
             ]
  , examples (simple 1.1)
             [ "1.1"
             , "1.10"
             , "01.10"
             ]
  , examples (simple 0.77)
             [ "0.77"
             , ".77"
             ]
  , examples (simple 100000)
             [ "100,000"
             , "100000"
             , "100K"
             , "100k"
             ]
  , examples (simple 3000000)
             [ "3M"
             , "3000K"
             , "3000000"
             , "3,000,000"
             ]
  , examples (simple 1200000)
             [ "1,200,000"
             , "1200000"
             , "1.2M"
             , "1200K"
             ]
  , examples (simple (-1200000))
             [ "- 1,200,000"
             , "-1200000"
             , "minus 1,200,000"
             , "-1.2M"
             , "-1200K"
             , "-.0012G"
             ]
  , examples (simple 5000)
             [ "5 tysięcy"
             , "pięć tysięcy"
             ]
  , examples (simple 122)
             [ "sto dwadzieścia dwa"
             ]
  , examples (simple 200000)
             [ "dwieście tysięcy"
             ]
  , examples (simple 21011)
             [ "dwadzieścia jeden tysięcy i jedenaście"
             , "dwadzieścia jeden tysięcy jedenaście"
             ]
  , examples (simple 721012)
             [ "siedemset dwadzieścia jeden tysięcy dwanaście"
             , "siedemset dwadzieścia jeden tysięcy i dwanaście"
             ]
  , examples (simple 65000000)
             [ "sześćdziesiąt pięć milionów"
             ]
  , examples (simple 31256721)
             [ "trzydzieści jeden milionów dwieście pięćdziesiąt sześć tysięcy siedemset dwadzieścia jeden"
             ]
  , examples (simple 15)
             [ "piętnasta"
             ]
  ]
