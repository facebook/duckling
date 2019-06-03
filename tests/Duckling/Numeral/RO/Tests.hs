-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


module Duckling.Numeral.RO.Tests
  ( tests
  ) where

import Data.String
import Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Duckling.Dimensions.Types
import Duckling.Locale
import Duckling.Numeral.RO.Corpus
import Duckling.Resolve
import Duckling.Testing.Asserts
import Duckling.Testing.Types

tests :: TestTree
tests = testGroup "RO Tests"
  [ makeCorpusTest [This Numeral] corpus
  , intersectTests
  ]

intersectTests :: TestTree
intersectTests = testCase "Intersect Test" $
  mapM_ (analyzedNTest context testOptions . withTargets [This Numeral])
    [ ("19 de milioane", 2) -- make sure ruleMultiplyDe only takes >= 20
    ]
  where
    context = testContext {locale = makeLocale RO Nothing}
