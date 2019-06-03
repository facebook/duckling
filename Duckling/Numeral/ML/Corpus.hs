-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.ML.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types


context :: Context
context = testContext {locale = makeLocale ML Nothing}

corpus :: Corpus
corpus = (context, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumeralValue 0)
             [ "പൂജ്യം"
             ]
  , examples (NumeralValue 1)
             [ "ഒന്ന്"
             ]
  , examples (NumeralValue 2)
             [ "രണ്ട്"
             ]
  , examples (NumeralValue 3)
             [ "മുന്ന്"
             ]
  , examples (NumeralValue 4)
             [ "നാല്"
             ]
  , examples (NumeralValue 5)
             [ "അഞ്ച്"
             ]
  , examples (NumeralValue 6)
             [ "ആറ്"
             ]
  , examples (NumeralValue 7)
             [ "ഏഴ്"
             ]
  , examples (NumeralValue 8)
             [ "എട്ട്"
             ]
  , examples (NumeralValue 9)
             [ "ഒൻപത്"
             ]
  , examples (NumeralValue 10)
             [ "പത്ത്"
             ]
  , examples (NumeralValue 11)
             [ "പതിനൊന്ന്"
             ]
  , examples (NumeralValue 12)
             [ "പന്ത്രണ്ട്"
             ]
  , examples (NumeralValue 13)
             [ "പതിമൂന്ന്"
             ]
  , examples (NumeralValue 19)
             [ "പത്തൊമ്പത്"
             ]
  , examples (NumeralValue 20)
             [ "ഇരുപത്"
             ]
  , examples (NumeralValue 21)
             [ "ഇരുപത്തിഒന്ന്"
             ]
  , examples (NumeralValue 22)
             [ "ഇരുപത്തിരണ്ട്"
             ]
  , examples (NumeralValue 26)
             [ "ഇരുപത്തിആറ്"
             ]
  , examples (NumeralValue 30)
             [ "മുപ്പത്"
             ]
   , examples (NumeralValue 33)
             [ "മുപ്പത്തിമുന്ന്"
             ]
  , examples (NumeralValue 50)
             [ "അമ്പത്"
             ]
  , examples (NumeralValue 51)
             [ "അമ്പത്തിഒന്ന്"
             ]
  ]
