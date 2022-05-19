-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


module Duckling.Duration.EN.Tests
  ( tests
  ) where

import Prelude
import Data.String
import Test.Tasty
import Test.Tasty.HUnit

import Duckling.Dimensions.Types
import Duckling.Duration.EN.Corpus
import Duckling.Testing.Asserts
import Duckling.Testing.Types hiding (examples)
import Duckling.Types (Range(..))

tests :: TestTree
tests = testGroup "EN Tests"
  [ makeCorpusTest [Seal Duration] corpus
  , makeNegativeCorpusTest [Seal Duration] negativeCorpus
  , rangeTests
  ]

rangeTests :: TestTree
rangeTests = testCase "Range Test" $
  mapM_ (analyzedRangeTest testContext testOptions . withTargets [Seal Duration]) xs
  where
    xs = [ ("1 hour 111565513", Range 0 6) -- ruleDurationHoursAndMinutes to accept valid minutes
         ]
