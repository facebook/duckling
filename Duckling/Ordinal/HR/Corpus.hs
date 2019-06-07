-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.

{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.HR.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Locale
import Duckling.Ordinal.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale HR Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (OrdinalData 3)
             [ "3."
             , "trece"
             , "treći"
             , "trećeg"
             ]
  , examples (OrdinalData 4)
             [ "4."
             , "4ti"
             , "4ta"
             , "četvrti"
             , "četvrta"
             , "četvrto"
             , "cetvrti"
             , "cetvrta"
             , "cetvrto"
             ]
  , examples (OrdinalData 6)
             [ "6."
             , "6ti"
             , "šesto"
             , "šestoga"
             , "sestog"
             ]
  ]
