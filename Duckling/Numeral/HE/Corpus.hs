-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.

{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.HE.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale HE Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumeralValue 0)
             [ "0"
             , "אפס"
             ]
  , examples (NumeralValue 1)
             [ "1"
             , "אחד"
             , "אחת"
             , "יחיד"
             ]
  , examples (NumeralValue 2)
             [ "2"
             , "שתיים"
             , "שניים"
             , "זוג"
             ]
  , examples (NumeralValue 33)
             [ "33"
             , "שלושים ושלוש"
             , "שלושים ושלושה"
             , "0033"
             ]
  , examples (NumeralValue 14)
             [ "14"
             , "ארבעה עשר"
             , "ארבע עשרה"
             ]
  , examples (NumeralValue 16)
             [ "16"
             , "ששה עשר"
             , "שש עשרה"
             ]
  , examples (NumeralValue 17)
             [ "17"
             , "שבעה עשר"
             , "שבע עשרה"
             ]
  , examples (NumeralValue 18)
             [ "18"
             , "שמונה עשר"
             , "שמונה עשרה"
             ]
  , examples (NumeralValue 1.1)
             [ "1.1"
             , "1.10"
             , "01.10"
             ]
  , examples (NumeralValue 0.5)
             [ "חצי"
             , "0.5"
             ]
  , examples (NumeralValue 0.77)
             [ "0.77"
             , ".77"
             ]
  , examples (NumeralValue 100000)
             [ "100,000"
             , "100000"
             ]
  , examples (NumeralValue 3000000)
             [ "3000000"
             , "3,000,000"
             ]
  , examples (NumeralValue 1200000)
             [ "1,200,000"
             , "1200000"
             ]
  , examples (NumeralValue (-1200000))
             [ "- 1,200,000"
             , "-1200000"
             , "מינוס 1,200,000"
             ]
  ]
