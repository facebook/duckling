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
  [ examples (NumeralValue 0)
             [ "பூஜ்ஜியம்"
             ]
  , examples (NumeralValue 1)
             [ "ஒன்று"
             ]
  , examples (NumeralValue 2)
             [ "இரண்டு"
             ]
  , examples (NumeralValue 3)
             [ "மூன்று"
             ]
  , examples (NumeralValue 4)
             [ "நான்கு"
             ]
  , examples (NumeralValue 5)
             [ "ஐந்து"
             ]
  , examples (NumeralValue 6)
             [ "ஆறு"
             ]
  , examples (NumeralValue 7)
             [ "ஏழு"
             ]
  , examples (NumeralValue 8)
             [ "எட்டு"
             ]
  , examples (NumeralValue 9)
             [ "ஒன்பது"
             ]
  , examples (NumeralValue 10)
             [ "பத்து"
             ]
  , examples (NumeralValue 11)
             [ "பதினொன்று"
             ]
  , examples (NumeralValue 12)
             [ "பன்னிரண்டு"
             ]
  , examples (NumeralValue 20)
             [ "இருபது"
             ]
  , examples (NumeralValue 21)
             [ "இருபத்திஒன்று"
             ]
  , examples (NumeralValue 22)
             [ "இருபத்திஇரண்டு"
             ]
  , examples (NumeralValue 26)
             [ "இருபத்திஆறு"
             ]
  , examples (NumeralValue 30)
             [ "முப்பது"
             ]
   , examples (NumeralValue 33)
             [ "முப்பத்துமூன்று"
             ]
  , examples (NumeralValue 50)
             [ "ஐம்பது"
             ]
  ]
