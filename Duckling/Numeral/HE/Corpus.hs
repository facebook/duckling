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
  [ examples (simple 0)
             [ "0"
             , "אפס"
             ]
  , examples (simple 1)
             [ "1"
             , "אחד"
             , "אחת"
             , "יחיד"
             ]
  , examples (simple 2)
             [ "2"
             , "שתיים"
             , "שניים"
             , "זוג"
             ]
  , examples (simple 33)
             [ "33"
             , "שלושים ושלוש"
             , "שלושים ושלושה"
             , "0033"
             ]
  , examples (simple 14)
             [ "14"
             , "ארבעה עשר"
             , "ארבע עשרה"
             ]
  , examples (simple 16)
             [ "16"
             , "ששה עשר"
             , "שש עשרה"
             ]
  , examples (simple 17)
             [ "17"
             , "שבעה עשר"
             , "שבע עשרה"
             ]
  , examples (simple 18)
             [ "18"
             , "שמונה עשר"
             , "שמונה עשרה"
             ]
  , examples (simple 1.1)
             [ "1.1"
             , "1.10"
             , "01.10"
             ]
  , examples (simple 0.5)
             [ "חצי"
             , "0.5"
             ]
  , examples (simple 0.77)
             [ "0.77"
             , ".77"
             ]
  , examples (simple 100000)
             [ "100,000"
             , "100000"
             ]
  , examples (simple 3000000)
             [ "3000000"
             , "3,000,000"
             ]
  , examples (simple 1200000)
             [ "1,200,000"
             , "1200000"
             ]
  , examples (simple (-1200000))
             [ "- 1,200,000"
             , "-1200000"
             , "מינוס 1,200,000"
             ]
  ]
