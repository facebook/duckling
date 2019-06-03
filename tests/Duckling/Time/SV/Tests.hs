-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Duckling.Time.SV.Tests
  ( tests ) where

import Data.String
import Prelude
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.HashSet as HashSet

import Duckling.Api
import Duckling.Dimensions.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Asserts
import Duckling.Testing.Types hiding (examples)
import Duckling.Time.Corpus
import Duckling.Time.SV.Corpus
import Duckling.TimeGrain.Types (Grain(..))

tests :: TestTree
tests = testGroup "SV Tests"
  [ makeCorpusTest [This Time] corpus
  , ambiguousTests
  ]

-- Ambiguous examples occur when multiple tokens have the same value,
-- but one is more flexible (they differ in the `values` field).
-- For example, "i morgonen" can both mean "this morning" and "in the morning".
ambiguousTests :: TestTree
ambiguousTests = testCase "Ambiguous Tests" $ mapM_ check xs
  where
    xs = examples (datetimeInterval ((2013, 2, 12, 4, 0, 0), (2013, 2, 12, 12, 0, 0)) Hour)
                  [ "i morgonen"
                  ]
    ctx = testContext {locale = makeLocale SV Nothing}
    dims = HashSet.singleton $ This Time
    check :: Example -> IO ()
    check (input, predicate) = case analyze input ctx testOptions dims of
      [] -> assertFailure $ "empty result on " ++ show input
      xs -> mapM_ (assertBool ("predicate on " ++ show input) . predicate ctx) xs
