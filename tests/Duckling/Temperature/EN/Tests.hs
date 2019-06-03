-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


module Duckling.Temperature.EN.Tests
  ( tests
  ) where

import Prelude
import Data.String
import Test.Tasty
import Test.Tasty.HUnit

import Duckling.Dimensions.Types
import Duckling.Temperature.EN.Corpus
import Duckling.Testing.Asserts
import Duckling.Testing.Types (testContext, testOptions)
import Duckling.Types (Range(..))

tests :: TestTree
tests = testGroup "EN Tests"
  [ makeCorpusTest [This Temperature] corpus
  , rangeTests
  ]

rangeTests :: TestTree
rangeTests = testCase "Range Test" $
  mapM_ (analyzedRangeTest testContext testOptions
          . withTargets [This Temperature]) xs
  where
    xs = [ ("between 40 and 30 degrees", Range 15 25 )
         , ("30 degrees degrees", Range 0 10 )
         ]
