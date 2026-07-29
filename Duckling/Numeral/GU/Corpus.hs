-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.GU.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale GU Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumeralValue 0)
             [ "શૂન્ય"
             , "૦"
             ]
  , examples (NumeralValue 1)
             [ "એક"
             ]
  , examples (NumeralValue 2)
             [ "બે"
             ]
  , examples (NumeralValue 3)
             [ "ત્રણ"
             ]
  , examples (NumeralValue 4)
             [ "ચાર"
             ]
  , examples (NumeralValue 5)
             [ "પાંચ"
             ]
  , examples (NumeralValue 6)
             [ "છ"
             ]
  , examples (NumeralValue 7)
             [ "સાત"
             ]
  , examples (NumeralValue 8)
             [ "આઠ"
             ]
  , examples (NumeralValue 9)
             [ "નવ"
             ]
  , examples (NumeralValue 10)
             [ "દસ"
             ]
  , examples (NumeralValue 1.5)
             [ "એક પોઇન્ટ પાંચ"
             , "1.5"
             , "૧.૫"
             ]
  ]
