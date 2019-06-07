-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.EL.Corpus
  ( corpus ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Ordinal.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale EL Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (OrdinalData 1)
             [ "πρώτος"
             , "1ος"
             , "1ου"
             , "πρώτων"
             ]
  , examples (OrdinalData 2)
             [ "δεύτερος"
             , "2ου"
             , "δευτέρου"
             ]
  , examples (OrdinalData 3)
             [ "τρίτος"
             , "3ης"
             ]
  , examples (OrdinalData 4)
             [ "τέταρτος"
             , "4ος"
             ]
  , examples (OrdinalData 8)
             [ "όγδοος"
             , "ογδόου"
             , "8ος"
             ]
  , examples (OrdinalData 25)
             [ "εικοστός πέμπτος"
             , "25ος"
             , "εικοστού πέμπτου"
             ]
  , examples (OrdinalData 31)
             [ "τριακοστός πρώτος"
             , "31ος"
             ]
  , examples (OrdinalData 42)
             [ "τεσσαρακοστός δεύτερος"
             , "42 ος"
             ]
  , examples (OrdinalData 77)
            [ "εβδομηκοστού εβδόμου"
            , "77ου"
            ]
  , examples (OrdinalData 90)
            [ "ενενηκοστός"
            , "90ος"
            ]
  ]
