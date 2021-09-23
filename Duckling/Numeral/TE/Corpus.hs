-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.TE.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale TE Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumeralValue 0)
             [ "సున్న"
             ]
  , examples (NumeralValue 1)
             [ "ఒకటి"
             ]
  , examples (NumeralValue 2)
             [ "రెండు"
             ]
  , examples (NumeralValue 3)
             [ "మూడు"
             ]
  , examples (NumeralValue 4)
             [ "నాలుగు"
             ]
  , examples (NumeralValue 5)
             [ "ఐదు"
             ]
  , examples (NumeralValue 6)
             [ "ఆరు"
             ]
  , examples (NumeralValue 7)
             [ "ఏడు"
             ]
  , examples (NumeralValue 8)
             [ "ఎనిమిది"
             ]
  , examples (NumeralValue 9)
             [ "తొమ్మిది"
             ]
  , examples (NumeralValue 10)
             [ "పది"
             ]
  , examples (NumeralValue 11)
             [ "పదకొండు"
             ]
  , examples (NumeralValue 12)
             [ "పన్నెండు"
             ]
  , examples (NumeralValue 13)
             [ "పదమూడు"
             ]
  , examples (NumeralValue 14)
             [ "పద్నాల్గు"
             ]
  , examples (NumeralValue 15)
             [ "పదిహేను"
             ]
  , examples (NumeralValue 16)
             [ "పదహారు"
             ]
  , examples (NumeralValue 17)
             [ "పదిహేడు"
             ]
  , examples (NumeralValue 18)
             [ "పద్దెనిమిది"
             ]
  , examples (NumeralValue 19)
             [ "పంతొమ్మిది"
             ]
  , examples (NumeralValue 20)
             [ "ఇరవై"
             ]
  , examples (NumeralValue 30)
             [ "ముప్పై"
             ]
   , examples (NumeralValue 40)
             [ "నలబై"
             ]
  , examples (NumeralValue 50)
             [ "యాబై"
             ]
  , examples (NumeralValue 60)
             [ "అరవై"
             ]
  , examples (NumeralValue 70)
             [ "డెబ్బై"
             ]
  , examples (NumeralValue 80)
             [ "ఎనబై"
             ]
  , examples (NumeralValue 90)
             [ "తొంబై"
             ]
  , examples (NumeralValue 100)
             [ "వంద"
             ]
  , examples (NumeralValue 1000)
             [ "వెయ్యి"
             ]
  , examples (NumeralValue 100000)
             [ "లక్ష"
             ]
  , examples (NumeralValue 10000000)
             [ "కోటి"
             ]
  ]
