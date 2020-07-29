-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.ET.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale ET Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple 0)
             [ "0"
             , "null"
             ]
  , examples (simple 1)
             [ "1"
             , "üks"
             ]
  , examples (simple 33)
             [ "33"
             , "Kolmkümmend kolm"
             ]
  , examples (simple 14)
             [ "14"
             , "neliteist"
             ]
  , examples (simple 16)
             [ "16"
             , "kuusteist"
             ]
  , examples (simple 17)
             [ "17"
             , "Seitseteist"
             ]
  , examples (simple 18)
             [ "18"
             , "kaheksateist"
             ]
  , examples (simple 1.1)
             [ "1.1"
             , "1.10"
             , "01.10"
             ]
  , examples (simple 0.77)
             [ "0.77"
             , ".77"
             ]
  , examples (simple 100000)
             [ "100,000"
             , "100000"
             , "100K"
             , "100k"
             , "100 000"
             ]
  , examples (simple 3000000)
             [ "3M"
             , "3000K"
             , "3000000"
             , "3,000,000"
             , "3 000 000"
             ]
  , examples (simple 1200000)
             [ "1,200,000"
             , "1200000"
             , "1.2M"
             , "1200K"
             , ".0012G"
             , "1 200 000"
             ]
  , examples (simple (-1200000))
             [ "- 1,200,000"
             , "-1200000"
             , "miinus 1,200,000"
             , "-1.2M"
             , "-1200K"
             , "-.0012G"
             ]
  , examples (simple 5000)
             [ "viis tuhat"
             ]
  , examples (simple 200000)
             [ "kakssada tuhat"
             ]
  , examples (simple 21011)
             [ "kakskümmend üks Tuhat üksteist"
             ]
  , examples (simple 721012)
             [ "seitsesada kakskümmend üks tuhat kaksteist"
             ]
  , examples (simple 31256721)
             [ "kolmkümmend üks miljonit kakssada viiskümmend kuus tuhat seitsesada kakskümmend üks"
             ]
  ]
