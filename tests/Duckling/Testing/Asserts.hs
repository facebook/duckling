-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE TupleSections #-}

module Duckling.Testing.Asserts
  ( analyzedTargetTest
  , analyzedFirstTest
  , analyzedAmbiguousTest
  , analyzedNTest
  , analyzedNothingTest
  , analyzedRangeTest
  , makeCorpusTest
  , makeNegativeCorpusTest
  , withTargets
  ) where

import Data.List (partition)
import Data.String
import Data.Text (Text)
import Prelude
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text

import Duckling.Api
import Duckling.Dimensions.Types
import Duckling.Resolve
import Duckling.Testing.Types
import Duckling.Types

newtype ExampleInput = ExampleInput Text

instance Show ExampleInput where
  show (ExampleInput input) = "\"" ++ Text.unpack input ++ "\""

withTargets :: [Seal Dimension] -> (Text, a) -> (Text, [Seal Dimension], a)
withTargets targets (input, expected) = (input, targets, expected)

analyzedTargetTest :: Context -> Options -> (Text, Seal Dimension) -> IO ()
analyzedTargetTest context options (input, target) =
  assertBool msg $ all (== target) dimensions
  where
    msg = "analyze " ++ show (ExampleInput input, [target])
          ++ "dimensions = " ++ show dimensions
    dimensions = flip map (analyze input context options $ HashSet.singleton target) $
      \(Resolved{node=Node{token=Token dimension _}}) -> Seal dimension

analyzedFirstTest :: Context -> Options ->
  (Text, [Seal Dimension], TestPredicate) -> IO ()
analyzedFirstTest context options (input, targets, predicate) =
  case tokens of
    [] -> assertFailure
      ("empty result on " ++ show (ExampleInput input, targets))
    (token:_) -> assertBool
      ("don't pass predicate on " ++ show (ExampleInput input)) $
      predicate context token
    where
      tokens = analyze input context options $ HashSet.fromList targets

analyzedAmbiguousTest :: Context -> Options ->
  (Text, [Seal Dimension], [TestPredicate]) -> IO ()
analyzedAmbiguousTest context options (input, targets, predicates) =
  case tokens of
    [] -> assertFailure
      ("empty result on " ++ show (ExampleInput input, targets))
    _ -> assertBool ("don't pass predicate on " ++ show (ExampleInput input)) $
      all (\predicate -> any (predicate context) tokens) predicates
    where
      tokens = analyze input context options $ HashSet.fromList targets

makeCorpusTest :: [Seal Dimension] -> Corpus -> TestTree
makeCorpusTest targets (context, options, xs) = testCase "Corpus Tests" $
  mapM_ check xs
  where
    dims = HashSet.fromList targets
    check :: Example -> IO ()
    check (input, predicate) =
      let tokens = analyze input context options dims in
      let inputRange = Range 0 $ Text.length input in
      let (fullRangeTokens, restTokens) =
            partition ((== inputRange) . range) tokens in
      case fullRangeTokens of
        [] -> case restTokens of
          [] -> assertFailure $ "empty result on " ++ show (ExampleInput input)
          (_:_:_) -> assertFailure $
            show (length restTokens) ++
            " tokens found for " ++
            show (ExampleInput input)
          _ -> assertFailure $ "don't fully match " ++ show (ExampleInput input)
        [token] -> assertBool
          ("don't pass predicate on " ++ show (ExampleInput input)) $
          predicate context token
        _ -> assertFailure $ show (length fullRangeTokens)
          ++ " different ambiguous parses on " ++ show (ExampleInput input)


makeNegativeCorpusTest :: [Seal Dimension] -> NegativeCorpus -> TestTree
makeNegativeCorpusTest targets (context, options, xs) =
  testCase "Negative Corpus Tests"
  $ mapM_ (analyzedNothingTest context options . (, targets)) xs

analyzedRangeTest :: Context -> Options -> (Text, [Seal Dimension], Range)
  -> IO ()
analyzedRangeTest context options  (input, targets, expRange) = case tokens of
  [] -> assertFailure $ "empty result on " ++ show (ExampleInput input)
  (_:_:_) -> assertFailure $
    show (length tokens) ++ " tokens found for " ++ show (ExampleInput input)
  (token:_) ->
    assertEqual ("wrong range for " ++ show (ExampleInput input))
    expRange (range token)
  where
    tokens = analyze input context options $ HashSet.fromList targets

analyzedNothingTest :: Context -> Options -> (Text, [Seal Dimension]) -> IO ()
analyzedNothingTest context options (input, targets) =
  analyzedNTest context options (input, targets, 0)

analyzedNTest :: Context -> Options -> (Text, [Seal Dimension], Int) -> IO ()
analyzedNTest context options (input, targets, n) =
  assertBool msg . (== n) $ length tokens
  where
    msg = "analyze " ++ show (ExampleInput input, targets)
          ++ "tokens= " ++ show tokens
    tokens = analyze input context options $ HashSet.fromList targets
