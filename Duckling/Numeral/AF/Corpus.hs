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
  [ examples (simple 0)
             [ "nul"
             , "geen"
             , "niks"
             ]
  , examples (simple 1)
             [ "een"
             ]
  , examples (simple 2)
             [ "twee"
             ]
  , examples (simple 3)
             [ "drie"
             ]
  , examples (simple 4)
             [ "vier"
             ]
  , examples (simple 5)
             [ "vyf"
             ]
  , examples (simple 6)
             [ "ses"
             ]
  , examples (simple 7)
             [ "sewe"
             ]
  , examples (simple 8)
             [ "agt"
             , "ag"
             ]
  , examples (simple 9)
             [ "nege"
             ]
  , examples (simple 10)
             [ "tien"
             ]
  , examples (simple 11)
             [ "elf"
             ]
  , examples (simple 12)
             [ "twaalf"
             , "dosyn"
             ]
  , examples (simple 14)
             [ "veertien"
             ]
  , examples (simple 15)
             [ "vyftien"
             ]
  , examples (simple 16)
             [ "sestien"
             ]
  , examples (simple 17)
             [ "sewentien"
             ]
  , examples (simple 19)
             [ "negentien"
             , "neentien"
             ]
  , examples (simple 20)
             [ "twintig"
             ]
  , examples (simple 22)
             [ "twee en twintig"
             ]
  , examples (simple 24)
             [ "vier en twintig"
             ]
  , examples (simple 26)
             [ "ses en twintig"
             ]
  , examples (simple 28)
             [ "agt en twintig"
             ]
  , examples (simple 33)
             [ "drie en dertig"
             ]
  , examples (simple 34)
             [ "vier en dertig"
             ]
  , examples (simple 50)
             [ "vyftig"
             ]
  , examples (simple 55)
             [ "vyf en vyftig"
             ]
  , examples (simple 1.1)
             [ "1,1"
             , "1,10"
             , "01,10"
             ]
  , examples (simple 0.77)
             [ "0,77"
             , ",77"
             ]
  , examples (simple 300)
             [ "3 honderd"
             , "drie honderd"
             ]
  , examples (simple 5000)
             [ "5 duisend"
             , "vyf duisend"
             ]
  , examples (simple 144)
             [ "een honderd vier en veertig"
             , "honderd vier en veertig"
             ]
  , examples (simple 122)
             [ "een honderd twee en twintig"
             , "honderd twee en twintig"
             ]
  , examples (simple 20000)
             [ "twintig duisend"
             ]
  , examples (simple 0.2)
             [ "0,2"
             , "1/5"
             , "2/10"
             , "3/15"
             , "20/100"
             ]
  ]
