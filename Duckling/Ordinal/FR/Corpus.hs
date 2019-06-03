-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.FR.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Locale
import Duckling.Ordinal.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale FR Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (OrdinalData 1)
             [ "1er"
             , "1ere"
             , "1ère"
             , "Premier"
             , "première"
             ]
  , examples (OrdinalData 2)
             [ "deuxieme"
             , "deuxième"
             , "2e"
             , "second"
             , "2eme"
             , "2ème"
             ]
  , examples (OrdinalData 11)
             [ "onzieme"
             , "onzième"
             , "11eme"
             , "11ème"
             , "11e"
             ]
  ]
