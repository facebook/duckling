-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Number.ET.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Lang
import Duckling.Number.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {lang = ET}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumberValue 0)
             [ "0"
             , "null"
             ]
  , examples (NumberValue 1)
             [ "1"
             , "üks"
             ]
  , examples (NumberValue 33)
             [ "33"
             , "Kolmkümmend kolm"
             ]
  , examples (NumberValue 14)
             [ "14"
             , "neliteist"
             ]
  , examples (NumberValue 16)
             [ "16"
             , "kuusteist"
             ]
  , examples (NumberValue 17)
             [ "17"
             , "Seitseteist"
             ]
  , examples (NumberValue 18)
             [ "18"
             , "kaheksateist"
             ]
  , examples (NumberValue 1.1)
             [ "1.1"
             , "1.10"
             , "01.10"
             ]
  , examples (NumberValue 0.77)
             [ "0.77"
             , ".77"
             ]
  , examples (NumberValue 100000)
             [ "100,000"
             , "100000"
             , "100K"
             , "100k"
             , "100 000"
             ]
  , examples (NumberValue 3000000)
             [ "3M"
             , "3000K"
             , "3000000"
             , "3,000,000"
             , "3 000 000"
             ]
  , examples (NumberValue 1200000)
             [ "1,200,000"
             , "1200000"
             , "1.2M"
             , "1200K"
             , ".0012G"
             , "1 200 000"
             ]
  , examples (NumberValue (-1200000))
             [ "- 1,200,000"
             , "-1200000"
             , "miinus 1,200,000"
             , "-1.2M"
             , "-1200K"
             , "-.0012G"
             ]
  , examples (NumberValue 5000)
             [ "viis tuhat"
             ]
  , examples (NumberValue 200000)
             [ "kakssada tuhat"
             ]
  , examples (NumberValue 21011)
             [ "kakskümmend üks Tuhat üksteist"
             ]
  , examples (NumberValue 721012)
             [ "seitsesada kakskümmend üks tuhat kaksteist"
             ]
  , examples (NumberValue 31256721)
             [ "kolmkümmend üks miljonit kakssada viiskümmend kuus tuhat seitsesada kakskümmend üks"
             ]
  ]
