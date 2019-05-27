-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Distance.DE.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Distance.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale DE Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Kilometre 3)
             [ "3 kilometer"
             , "3 km"
             , "3km"
             , "3,0 km"
             ]
  , examples (simple Mile 8)
             [ "acht meilen"
             , "8 meilen"
             ]
  , examples (simple Metre 9)
             [ "9m"
             ]
  , examples (simple Centimetre 2)
             [ "2cm"
             , "2 zentimeter"
             ]
  , examples (simple Inch 5)
             [ "5''"
             , "fünf zoll"
             , "5\""
             ]
  , examples (simple Metre 1.87)
             [ "1,87 meter"
             ]
  , examples (between Kilometre (3, 5))
             [ "zwischen 3 und 5 kilometern"
             , "von 3km bis 5km"
             , "um die 3-5 kilometer"
             , "etwa 3km-5km"
             , "3-5 kilometer"
             ]
  , examples (under Mile 3.5)
             [ "unter 3,5 meilen"
             , "weniger als 3,5meilen"
             --, "niedriger als dreikommafünf meilen"
             ]
  , examples (above Inch 5)
             [ "mehr als fünf zoll"
             , "mindestens 5''"
             , "über 5\""
             ]
  , examples (between Millimetre (5, 6))
             [ "zwischen 5 und sechs Millimetern"
             , "zwischen 5 und sechs millimeter"
             , "5-6 mm"
             ]
  ]
