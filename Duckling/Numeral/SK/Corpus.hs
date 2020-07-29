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
  [ examples (simple 0)
             [ "0"
             , "nula"
             ]
  , examples (simple 1)
             [ "1"
             , "jeden"
             , "jedna"
             ]
  , examples (simple 2)
             [ "2"
             , "dva"
             , "dve"
             ]
  , examples (simple 7)
             [ "7"
             , "sedem"
             ]
  , examples (simple 14)
             [ "14"
             , "štrnásť"
             ]
  , examples (simple 16)
             [ "16"
             , "šestnásť"
             ]
  , examples (simple 17)
             [ "17"
             , "sedemnásť"
             ]
  , examples (simple 18)
             [ "18"
             , "osemnásť"
             ]
  , examples (simple 20)
             [ "20"
             , "dvadsať"
             ]
  , examples (simple 1.1)
             [ "1,1"
             , "1,10"
             , "01,10"
             ]
  , examples (simple 0.77)
             [ "0,77"
             , ",77"
             ]
  , examples (simple 100000)
             [ "100.000"
             , "100000"
             , "100K"
             , "100k"
             ]
  , examples (simple 3000000)
             [ "3M"
             , "3000K"
             , "3000000"
             , "3.000.000"
             ]
  , examples (simple 1200000)
             [ "1.200.000"
             , "1200000"
             , "1,2M"
             , "1200K"
             , ",0012G"
             ]
  , examples (simple (-1200000))
             [ "- 1.200.000"
             , "-1200000"
             , "mínus 1.200.000"
             , "-1,2M"
             , "-1200K"
             , "-,0012G"
             ]
  , examples (simple 5000)
             [ "5 tisíc"
             , "päť tisíc"
             ]
  ]
