-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Number.PL.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Lang
import Duckling.Number.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {lang = PL}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumberValue 0)
             [ "0"
             , "nic"
             , "zero"
             ]
  , examples (NumberValue 1)
             [ "1"
             , "jeden"
             , "pojedynczy"
             ]
  , examples (NumberValue 2)
             [ "2"
             , "dwa"
             , "para"
             ]
  , examples (NumberValue 33)
             [ "33"
             , "trzydzieści trzy"
             , "0033"
             ]
  , examples (NumberValue 14)
             [ "14"
             , "czternaście"
             ]
  , examples (NumberValue 16)
             [ "16"
             , "szesnaście"
             ]
  , examples (NumberValue 17)
             [ "17"
             , "siedemnaście"
             ]
  , examples (NumberValue 18)
             [ "18"
             , "osiemnaście"
             ]
  , examples (NumberValue 1.1)
             [ "1.1"
             , "1.10"
             , "01.10"
             ]
  , examples (NumberValue 0.77)
             [ "0.77"
             , ".77"
             ]
  , examples (NumberValue 100000)
             [ "100,000"
             , "100000"
             , "100K"
             , "100k"
             ]
  , examples (NumberValue 3000000)
             [ "3M"
             , "3000K"
             , "3000000"
             , "3,000,000"
             ]
  , examples (NumberValue 1200000)
             [ "1,200,000"
             , "1200000"
             , "1.2M"
             , "1200K"
             ]
  , examples (NumberValue (-1200000))
             [ "- 1,200,000"
             , "-1200000"
             , "minus 1,200,000"
             , "-1.2M"
             , "-1200K"
             , "-.0012G"
             ]
  , examples (NumberValue 5000)
             [ "5 tysięcy"
             , "pięć tysięcy"
             ]
  , examples (NumberValue 122)
             [ "sto dwadzieścia dwa"
             ]
  , examples (NumberValue 200000)
             [ "dwieście tysięcy"
             ]
  , examples (NumberValue 21011)
             [ "dwadzieścia jeden tysięcy i jedenaście"
             , "dwadzieścia jeden tysięcy jedenaście"
             ]
  , examples (NumberValue 721012)
             [ "siedemset dwadzieścia jeden tysięcy dwanaście"
             , "siedemset dwadzieścia jeden tysięcy i dwanaście"
             ]
  , examples (NumberValue 65000000)
             [ "sześćdziesiąt pięć milionów"
             ]
  , examples (NumberValue 31256721)
             [ "trzydzieści jeden milionów dwieście pięćdziesiąt sześć tysięcy siedemset dwadzieścia jeden"
             ]
  , examples (NumberValue 15)
             [ "piętnasta"
             ]
  ]
