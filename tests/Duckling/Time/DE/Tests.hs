-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


module Duckling.Time.DE.Tests
  ( tests ) where

import Data.String
import Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Duckling.Dimensions.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Asserts
import Duckling.Testing.Types (testContext, testOptions)
import Duckling.Time.DE.Corpus
import Duckling.Types (Range(..))

tests :: TestTree
tests = testGroup "DE Tests"
  [ makeCorpusTest [This Time] corpus
  , makeNegativeCorpusTest [This Time] negativeCorpus
  , rangeTests
  ]

rangeTests :: TestTree
rangeTests = testCase "Range Test" $
  mapM_ (analyzedRangeTest (testContext{locale = makeLocale DE Nothing})
         testOptions . withTargets [This Time]) xs
  where
    xs = [ ("Wir treffen uns am 17 Uhr am KiLa.", Range 16 25)
         ]
