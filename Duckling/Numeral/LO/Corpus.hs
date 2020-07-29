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
  [ examples (simple 0)
             [ "ສູນ"
             ]
  , examples (simple 1)
             [ "ໜຶ່ງ"
             ]
  , examples (simple 2)
             [ "ສອງ"
             ]
  , examples (simple 3)
             [ "ສາມ"
             ]
  , examples (simple 4)
             [ "ສີ່"
             ]
  , examples (simple 5)
             [ "ຫ້າ"
             ]
  , examples (simple 6)
             [ "ຫົກ"
             ]
  , examples (simple 7)
             [ "ເຈັດ"
             ]
  , examples (simple 8)
             [ "ແປດ"
             ]
  , examples (simple 9)
             [ "ເກົ້າ"
             ]
  , examples (simple 11)
             [ "ສິບເອັດ"
             ]
  , examples (simple 15)
             [ "ສິບຫ້າ"
             ]
  , examples (simple 17)
             [ "ສິບເຈັດ"
             ]
  , examples (simple 22)
             [ "ຊາວສອງ"
             ]
  , examples (simple 24)
             [ "ຊາວສີ່"
             ]
  , examples (simple 26)
             [ "ຊາວຫົກ"
             ]
  , examples (simple 28)
             [ "ຊາວແປດ"
             ]
  , examples (simple 34)
             [ "ສາມສິບສີ່"
             ]
  , examples (simple 10)
             [ "ສິບ"
             ]
  , examples (simple 20)
             [ "ຊາວ"
             ]
  , examples (simple 50)
             [ "ຫ້າສິບ"
             ]
  ]
