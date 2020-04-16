-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.AF.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus =
  ( testContext {locale = makeLocale AF Nothing}
  , testOptions
  , allExamples
  )

allExamples :: [Example]
allExamples = concat
  [ examples (NumeralValue 0)
             [ "nul"
             , "geen"
             , "niks"
             ]
  , examples (NumeralValue 1)
             [ "een"
             ]
  , examples (NumeralValue 2)
             [ "twee"
             ]
  , examples (NumeralValue 3)
             [ "drie"
             ]
  , examples (NumeralValue 4)
             [ "vier"
             ]
  , examples (NumeralValue 5)
             [ "vyf"
             ]
  , examples (NumeralValue 6)
             [ "ses"
             ]
  , examples (NumeralValue 7)
             [ "sewe"
             ]
  , examples (NumeralValue 8)
             [ "agt"
             , "ag"
             ]
  , examples (NumeralValue 9)
             [ "nege"
             ]
  , examples (NumeralValue 10)
             [ "tien"
             ]
  , examples (NumeralValue 11)
             [ "elf"
             ]
  , examples (NumeralValue 12)
             [ "twaalf"
             , "dosyn"
             ]
  , examples (NumeralValue 14)
             [ "veertien"
             ]
  , examples (NumeralValue 15)
             [ "vyftien"
             ]
  , examples (NumeralValue 16)
             [ "sestien"
             ]
  , examples (NumeralValue 17)
             [ "sewentien"
             ]
  , examples (NumeralValue 19)
             [ "negentien"
             , "neentien"
             ]
  , examples (NumeralValue 20)
             [ "twintig"
             ]
  , examples (NumeralValue 22)
             [ "twee en twintig"
             ]
  , examples (NumeralValue 24)
             [ "vier en twintig"
             ]
  , examples (NumeralValue 26)
             [ "ses en twintig"
             ]
  , examples (NumeralValue 28)
             [ "agt en twintig"
             ]
  , examples (NumeralValue 33)
             [ "drie en dertig"
             ]
  , examples (NumeralValue 34)
             [ "vier en dertig"
             ]
  , examples (NumeralValue 50)
             [ "vyftig"
             ]
  , examples (NumeralValue 55)
             [ "vyf en vyftig"
             ]
  , examples (NumeralValue 1.1)
             [ "1,1"
             , "1,10"
             , "01,10"
             ]
  , examples (NumeralValue 0.77)
             [ "0,77"
             , ",77"
             ]
  , examples (NumeralValue 300)
             [ "3 honderd"
             , "drie honderd"
             ]
  , examples (NumeralValue 5000)
             [ "5 duisend"
             , "vyf duisend"
             ]
  , examples (NumeralValue 144)
             [ "een honderd vier en veertig"
             , "honderd vier en veertig"
             ]
  , examples (NumeralValue 122)
             [ "een honderd twee en twintig"
             , "honderd twee en twintig"
             ]
  , examples (NumeralValue 20000)
             [ "twintig duisend"
             ]
  , examples (NumeralValue 0.2)
             [ "0,2"
             , "1/5"
             , "2/10"
             , "3/15"
             , "20/100"
             ]
  ]
