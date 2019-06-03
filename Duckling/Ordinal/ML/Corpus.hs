-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.ML.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Ordinal.Types
import Duckling.Resolve
import Duckling.Testing.Types

context :: Context
context = testContext {locale = makeLocale ML Nothing}

corpus :: Corpus
corpus = (context, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (OrdinalData 1)
             [ "ഒന്നാം"
             , "1."
             ]
  , examples (OrdinalData 2)
             [ "രണ്ടാം"
             , "2."
             ]
  , examples (OrdinalData 3)
             [ "മൂന്നാം"
             , "3."
             ]
  , examples (OrdinalData 4)
             [ "നാലാം"
             , "4."
             ]
  , examples (OrdinalData 5)
             [ "അഞ്ചാം"
             , "5."
             ]
  , examples (OrdinalData 6)
             [ "ആറാം"
             , "6."
             ]
  , examples (OrdinalData 7)
             [ "ഏഴാം"
             , "7."
             ]
  , examples (OrdinalData 8)
             [ "എട്ടാം"
             , "8."
             ]
  , examples (OrdinalData 9)
             [ "ഒമ്പതാം"
             , "9."
             ]
  , examples (OrdinalData 10)
             [ "പത്താം"
             , "10."
             ]
  , examples (OrdinalData 11)
             [ "പതിനൊന്നാം"
             , "11."
             ]
  , examples (OrdinalData 12)
             [ "പന്ത്രണ്ടാം"
             , "12."
             ]
  , examples (OrdinalData 20)
             [ "ഇരുപതാം"
             , "20."
             ]
  , examples (OrdinalData 22)
             [ "ഇരുപത്തിരണ്ടാം"
             , "22."
             ]
  , examples (OrdinalData 26)
             [ "ഇരുപത്തിആറാം"
             , "26."
             ]
  , examples (OrdinalData 30)
             [ "മുപ്പത്തഞ്ചാം"
             , "30."
             ]
   , examples (OrdinalData 33)
             [ "മുപ്പത്തിമൂന്നാം"
             , "33."
             ]
  , examples (OrdinalData 50)
             [ "അമ്പതാം"
             , "50."
             ]
   , examples (OrdinalData 54)
              [ "അമ്പത്തിനാലാം"
              , "54."
              ]
   , examples (OrdinalData 65)
              [ "അറുപത്തിഅഞ്ചാം"
              , "65."
              ]
   , examples (OrdinalData 76)
              [ "എഴുപത്തിആറാം"
              , "76."
              ]
   , examples (OrdinalData 87)
              [ "എൺപത്തിഏഴാം"
              , "87."
              ]
  ]
