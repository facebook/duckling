-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE TupleSections #-}

module Duckling.Testing.Asserts
  ( analyzedTargetTest
  , analyzedFirstTest
  , analyzedNTest
  , analyzedNothingTest
  , analyzedRangeTest
  , makeCorpusTest
  , makeNegativeCorpusTest
  , withTargets
  ) where

import Data.String
import Data.Text (Text)
import Prelude
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text

import Duckling.Api
import Duckling.Dimensions.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types
import Duckling.Types

withTargets :: [Some Dimension] -> (Text, a) -> (Text, [Some Dimension], a)
withTargets targets (input, expected) = (input, targets, expected)

analyzedTargetTest :: Context -> (Text, Some Dimension) -> IO ()
analyzedTargetTest context (input, target) =
  assertBool msg $ all (== target) dimensions
  where
    msg = "analyze " ++ show (input, [target])
          ++ "dimensions = " ++ show dimensions
    dimensions = flip map (analyze input context $ HashSet.singleton target) $
      \(Resolved{node=Node{token=Token dimension _}}) -> This dimension

analyzedFirstTest :: Context -> (Text, [Some Dimension], TestPredicate) -> IO ()
analyzedFirstTest context (input, targets, predicate) =
  case tokens of
    [] -> assertFailure ("empty result on " ++ show (input, targets))
    (token:_) -> assertBool ("don't pass predicate on " ++ show input) $
      predicate context token
    where
      tokens = analyze input context $ HashSet.fromList targets

makeCorpusTest :: [Some Dimension] -> Corpus -> TestTree
makeCorpusTest targets (context, xs) = testCase "Corpus Tests" $ mapM_ check xs
  where
    dims = HashSet.fromList targets
    check :: Example -> IO ()
    check (input, predicate) = let tokens = analyze input context dims in
      case tokens of
        [] -> assertFailure $ "empty result on " ++ show input
        (_:_:_) -> assertFailure $
          show (length tokens) ++ " tokens found for " ++ show input
        (token:_) -> do
          assertEqual ("don't fully match " ++ show input)
            (Range 0 (Text.length input)) (range token)
          assertBool ("don't pass predicate on " ++ show input) $
            predicate context token

makeNegativeCorpusTest :: [Some Dimension] -> NegativeCorpus -> TestTree
makeNegativeCorpusTest targets (context, xs) = testCase "Negative Corpus Tests" $
  mapM_ (analyzedNothingTest context . (, targets)) xs

analyzedRangeTest :: Context -> (Text, [Some Dimension], Range) -> IO ()
analyzedRangeTest context (input, targets, expRange) = case tokens of
  [] -> assertFailure $ "empty result on " ++ show input
  (_:_:_) -> assertFailure $
    show (length tokens) ++ " tokens found for " ++ show input
  (token:_) ->
    assertEqual ("wrong range for " ++ show input) expRange (range token)
  where
    tokens = analyze input context $ HashSet.fromList targets

analyzedNothingTest :: Context -> (Text, [Some Dimension]) -> IO ()
analyzedNothingTest context (input, targets) =
  analyzedNTest context (input, targets, 0)

analyzedNTest :: Context -> (Text, [Some Dimension], Int) -> IO ()
analyzedNTest context (input, targets, n) =
  assertBool msg . (== n) $ length tokens
  where
    msg = "analyze " ++ show (input, targets)
          ++ "tokens= " ++ show tokens
    tokens = analyze input context $ HashSet.fromList targets
