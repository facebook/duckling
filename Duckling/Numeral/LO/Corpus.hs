-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.LO.Corpus
  ( corpus ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale LO Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumeralValue 0)
             [ "ສູນ"
             ]
  , examples (NumeralValue 1)
             [ "ໜຶ່ງ"
             ]
  , examples (NumeralValue 2)
             [ "ສອງ"
             ]
  , examples (NumeralValue 3)
             [ "ສາມ"
             ]
  , examples (NumeralValue 4)
             [ "ສີ່"
             ]
  , examples (NumeralValue 5)
             [ "ຫ້າ"
             ]
  , examples (NumeralValue 6)
             [ "ຫົກ"
             ]
  , examples (NumeralValue 7)
             [ "ເຈັດ"
             ]
  , examples (NumeralValue 8)
             [ "ແປດ"
             ]
  , examples (NumeralValue 9)
             [ "ເກົ້າ"
             ]
  , examples (NumeralValue 11)
             [ "ສິບເອັດ"
             ]
  , examples (NumeralValue 15)
             [ "ສິບຫ້າ"
             ]
  , examples (NumeralValue 17)
             [ "ສິບເຈັດ"
             ]
  , examples (NumeralValue 22)
             [ "ຊາວສອງ"
             ]
  , examples (NumeralValue 24)
             [ "ຊາວສີ່"
             ]
  , examples (NumeralValue 26)
             [ "ຊາວຫົກ"
             ]
  , examples (NumeralValue 28)
             [ "ຊາວແປດ"
             ]
  , examples (NumeralValue 34)
             [ "ສາມສິບສີ່"
             ]
  , examples (NumeralValue 10)
             [ "ສິບ"
             ]
  , examples (NumeralValue 20)
             [ "ຊາວ"
             ]
  , examples (NumeralValue 50)
             [ "ຫ້າສິບ"
             ]
  ]
