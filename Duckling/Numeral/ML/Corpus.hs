-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.ML.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types


context :: Context
context = testContext {locale = makeLocale ML Nothing}

corpus :: Corpus
corpus = (context, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple 0)
             [ "പൂജ്യം"
             ]
  , examples (simple 1)
             [ "ഒന്ന്"
             ]
  , examples (simple 2)
             [ "രണ്ട്"
             ]
  , examples (simple 3)
             [ "മുന്ന്"
             ]
  , examples (simple 4)
             [ "നാല്"
             ]
  , examples (simple 5)
             [ "അഞ്ച്"
             ]
  , examples (simple 6)
             [ "ആറ്"
             ]
  , examples (simple 7)
             [ "ഏഴ്"
             ]
  , examples (simple 8)
             [ "എട്ട്"
             ]
  , examples (simple 9)
             [ "ഒൻപത്"
             ]
  , examples (simple 10)
             [ "പത്ത്"
             ]
  , examples (simple 11)
             [ "പതിനൊന്ന്"
             ]
  , examples (simple 12)
             [ "പന്ത്രണ്ട്"
             ]
  , examples (simple 13)
             [ "പതിമൂന്ന്"
             ]
  , examples (simple 19)
             [ "പത്തൊമ്പത്"
             ]
  , examples (simple 20)
             [ "ഇരുപത്"
             ]
  , examples (simple 21)
             [ "ഇരുപത്തിഒന്ന്"
             ]
  , examples (simple 22)
             [ "ഇരുപത്തിരണ്ട്"
             ]
  , examples (simple 26)
             [ "ഇരുപത്തിആറ്"
             ]
  , examples (simple 30)
             [ "മുപ്പത്"
             ]
   , examples (simple 33)
             [ "മുപ്പത്തിമുന്ന്"
             ]
  , examples (simple 50)
             [ "അമ്പത്"
             ]
  , examples (simple 51)
             [ "അമ്പത്തിഒന്ന്"
             ]
  ]
