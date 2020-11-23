-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.SK.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus =
  ( testContext {locale = makeLocale SK Nothing}
  , testOptions
  , allExamples
  )

allExamples :: [Example]
allExamples = concat
  [ examples (NumeralValue 0)
             [ "0"
             , "nula"
             ]
  , examples (NumeralValue 1)
             [ "1"
             , "jeden"
             , "jedna"
             ]
  , examples (NumeralValue 2)
             [ "2"
             , "dva"
             , "dve"
             ]
  , examples (NumeralValue 7)
             [ "7"
             , "sedem"
             ]
  , examples (NumeralValue 14)
             [ "14"
             , "štrnásť"
             ]
  , examples (NumeralValue 16)
             [ "16"
             , "šestnásť"
             ]
  , examples (NumeralValue 17)
             [ "17"
             , "sedemnásť"
             ]
  , examples (NumeralValue 18)
             [ "18"
             , "osemnásť"
             ]
  , examples (NumeralValue 20)
             [ "20"
             , "dvadsať"
             ]
  , examples (NumeralValue 1.1)
             [ "1,1"
             , "1,10"
             , "01,10"
             ]
  , examples (NumeralValue 0.77)
             [ "0,77"
             , ",77"
             ]
  , examples (NumeralValue 100000)
             [ "100.000"
             , "100000"
             , "100K"
             , "100k"
             ]
  , examples (NumeralValue 3000000)
             [ "3M"
             , "3000K"
             , "3000000"
             , "3.000.000"
             ]
  , examples (NumeralValue 1200000)
             [ "1.200.000"
             , "1200000"
             , "1,2M"
             , "1200K"
             , ",0012G"
             ]
  , examples (NumeralValue (-1200000))
             [ "- 1.200.000"
             , "-1200000"
             , "mínus 1.200.000"
             , "-1,2M"
             , "-1200K"
             , "-,0012G"
             ]
  , examples (NumeralValue 5000)
             [ "5 tisíc"
             , "päť tisíc"
             ]
  ]
