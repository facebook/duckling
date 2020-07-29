-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.TA.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale TA Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple 0)
             [ "பூஜ்ஜியம்"
             ]
  , examples (simple 1)
             [ "ஒன்று"
             ]
  , examples (simple 2)
             [ "இரண்டு"
             ]
  , examples (simple 3)
             [ "மூன்று"
             ]
  , examples (simple 4)
             [ "நான்கு"
             ]
  , examples (simple 5)
             [ "ஐந்து"
             ]
  , examples (simple 6)
             [ "ஆறு"
             ]
  , examples (simple 7)
             [ "ஏழு"
             ]
  , examples (simple 8)
             [ "எட்டு"
             ]
  , examples (simple 9)
             [ "ஒன்பது"
             ]
  , examples (simple 10)
             [ "பத்து"
             ]
  , examples (simple 11)
             [ "பதினொன்று"
             ]
  , examples (simple 12)
             [ "பன்னிரண்டு"
             ]
  , examples (simple 20)
             [ "இருபது"
             ]
  , examples (simple 21)
             [ "இருபத்திஒன்று"
             ]
  , examples (simple 22)
             [ "இருபத்திஇரண்டு"
             ]
  , examples (simple 26)
             [ "இருபத்திஆறு"
             ]
  , examples (simple 30)
             [ "முப்பது"
             ]
   , examples (simple 33)
             [ "முப்பத்துமூன்று"
             ]
  , examples (simple 50)
             [ "ஐம்பது"
             ]
  ]
