-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.FA.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale FA Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumeralValue 0)
             [ "0"
             , "۰"
             , "صفر"
             ]
  , examples (NumeralValue 1)
             [ "1"
             , "۱"
             , "یک"
             ]
  , examples (NumeralValue 11)
             [ "یازده"
             ]
  , examples (NumeralValue 17)
             [ "هفده"
             ]
  , examples (NumeralValue 21)
             [ "بیست و یک"
             ]
  , examples (NumeralValue 23)
             [ "بیست و سه"
             ]
  , examples (NumeralValue 70)
             [ "هفتاد"
             ]
  , examples (NumeralValue 71)
             [ "هفتاد و یک"
             ]
  , examples (NumeralValue 78)
             [ "هفتاد و هشت"
             ]
  , examples (NumeralValue 73)
             [ "هفتاد و سه"
             ]
  , examples (NumeralValue 80)
             [ "هشتاد"
             ]
  , examples (NumeralValue 81)
             [ "هشتاد و یک"
             ]
  , examples (NumeralValue 82)
             [ "هشتاد و دو"
             ]
  , examples (NumeralValue 90)
             [ "نود"
             ]
  , examples (NumeralValue 91)
             [ "نود و یک"
             ]
  , examples (NumeralValue 92)
             [ "نود و دو"
             ]
  , examples (NumeralValue 99)
             [ "نود و نه"
             ]
  , examples (NumeralValue 33)
             [ "33"
             , "۳۳"
             , "سی و سه"
             ]
  , examples (NumeralValue 118)
             [ "صد و هجده"
             ]
  , examples (NumeralValue 4020)
             [ "چهار هزار و بیست"
             ]
  , examples (NumeralValue 100000)
             [ "صد هزار"
             , "100000"
             , "۱۰۰۰۰۰"
             ]
  , examples (NumeralValue 3000000)
             [ "سه میلیون"
             , "3000000"
             ]
  ]
