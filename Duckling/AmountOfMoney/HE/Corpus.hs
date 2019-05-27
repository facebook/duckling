-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.

{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.HE.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.AmountOfMoney.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus =
  (testContext { locale = makeLocale HE Nothing }, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple ILS 10)
             [ "עשר שקל"
             , "עשרה שקלים"
             , "עשר ש״ח"
             , "עשר שח"
             , "10₪"
             ]
  , examples (simple ILS 10000)
             [ "עשר אלף שקל"
             , "10000 שקלים"
             , "10 אשח"
             , "10 אש״ח"
             ]
  , examples (simple ILS 10000000)
             [ "10 מיליון שקלים"
             , "10 משח"
             , "10 מש״ח"
             ]
  , examples (simple ILS 0.01)
             [ "אגורה"
             ]
  , examples (simple ILS 1)
             [ "שקל"
             , "שקל אחד"
             , "שקל בודד"
             , "100 אגורות"
             ]
  , examples (simple ILS 0.05)
             [ "חמש אגורות"
             ]
  , examples (simple ILS 0)
             [ "אפס ש״ח"
             ]
  , examples (simple ILS 1.5)
             [ "שקל וחצי"
             , "1.5 ש״ח"
             ]
  , examples (simple ILS 2)
             [ "שנקל"
             , "2 שקל"
             ]
  , examples (simple Dollar 10)
             [ "$10"
             , "10$"
             , "עשר דולר"
             , "עשרה דולר"
             , "עשרה דולרים"
             , "עשר דולרים"
             ]
  , examples (simple Dollar 20)
             [ "עשרים דולר"
             ]
  , examples (simple Dollar 2.23)
             [ "2 דולר ו23 סנט"
             ]
  , examples (simple EUR 20)
             [ "20€"
             , "20 יורו"
             , "EUR 20"
             , "20 אירו"
             ]
  , examples (simple EUR 29.99)
             [ "29.99 יורו"
             ]
  , examples (simple Pound 9)
             [ "£9"
             , "תשע פאונד"
             ]
  , examples (simple GBP 1)
             [ "לירה שטרלינג"
             ]
  , examples (simple GBP 10)
             [ "10 לירות שטרלינג"
             ]
  , examples (between Dollar (10, 20))
             [ "מ10 עד 20 דולר"
             , "מעשר עד עשרים דולר"
             , "בין 10 ל20 דולר"
             , "10$-20$"
             , "10-20$"
             , "10-20 דולר"
             , "בין 10 דולר ל20 דולר"
             ]
  , examples (under EUR 7)
             [ "פחות מ7 יורו"
             , "עד 7 יורו"
             , "לא יותר מ7 יורו"
             , "מתחת ל7 יורו"
             , "לא מעל 7 יורו"
             ]
  , examples (above Dollar 5)
             [ "יותר מ5 דולר"
             , "מעל 5 דולר"
             , "מ5 דולר"
             , "לא פחות מ5 דולר"
             , "לא מתחת ל5 דולר"
             ]
    ]
