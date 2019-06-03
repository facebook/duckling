-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


module Duckling.Distance.ZH.Tests
  ( tests ) where

import Data.String
import Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Duckling.Dimensions.Types
import Duckling.Distance.Types
import Duckling.Distance.ZH.Corpus
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Asserts
import Duckling.Testing.Types

tests :: TestTree
tests = testGroup "ZH Tests"
  [ makeCorpusTest [This Distance] corpus
  , ambiguousTests
  ]

ambiguousTests :: TestTree
ambiguousTests = testCase "Ambiguous Tests" $
  analyzedAmbiguousTest context testOptions
    (testText, [This Distance], predicates)
  where
    context = testContext {locale = makeLocale ZH Nothing}
    testText = "3千米"
    predicates = simpleCheck <$>
      [ simple Kilometre 3.0
      , simple Metre 3000.0
      ]
