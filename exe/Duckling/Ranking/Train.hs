-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE RecordWildCards #-}

module Duckling.Ranking.Train
  ( makeClassifiers
  ) where

import Data.HashSet (HashSet)
import Prelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.List as List

import Duckling.Engine
import Duckling.Ranking.Extraction
import Duckling.Ranking.Types
import Duckling.Resolve
import Duckling.Testing.Types
import Duckling.Types

-- -----------------------------------------------------------------
-- Probabilistic layer
-- Naive Bayes classifier with Laplace smoothing
-- Train one classifier per rule, based on the test corpus.

makeClassifiers :: [Rule] -> Corpus -> Classifiers
makeClassifiers rules corpus = HashMap.map train $ makeDataset rules corpus

-- | Train a classifier for a single rule
train :: [Datum] -> Classifier
train datums = Classifier {okData = okClass, koData = koClass}
  where
    total = List.length datums
    (ok, ko) = List.partition snd datums
    merge :: [BagOfFeatures] -> BagOfFeatures -> BagOfFeatures
    merge xs m = List.foldl' (HashMap.unionWith (+)) m xs
    okCounts = merge (map fst ok) HashMap.empty
    koCounts = merge (map fst ko) HashMap.empty
    vocSize = HashMap.size $ HashMap.union okCounts koCounts
    okClass = makeClass okCounts total (List.length ok) vocSize
    koClass = makeClass koCounts total (List.length ko) vocSize

-- | Compute prior and likelihoods log-probabilities for one class.
makeClass :: BagOfFeatures -> Int -> Int -> Int -> ClassData
makeClass feats total classTotal vocSize = ClassData
  { prior = prior
  , unseen = unseen
  , likelihoods = likelihoods
  , n = classTotal
  }
  where
    prior = log $ fromIntegral classTotal / fromIntegral total
    denum = vocSize + sum (HashMap.elems feats)
    unseen = log $ 1.0 / (fromIntegral denum + 1.0)
    likelihoods = HashMap.map (\x ->
      log $ (fromIntegral x + 1.0) / fromIntegral denum
      ) feats

-- | Augment the dataset with one example.
-- | Add all the nodes contributing to the resolutions of the input sentence.
-- | Classes:
-- | 1) True (node contributed to a token passing test predicate)
-- | 2) False (node didn't contribute to any passing tokens)
makeDataset1 :: [Rule] -> Context -> Options -> Dataset -> Example -> Dataset
makeDataset1 rules context options dataset (sentence, predicate) = dataset'
  where
    tokens = parseAndResolve rules sentence context options
    (ok, ko) = List.partition (predicate context) tokens
    subnodes :: Node -> HashSet Node
    subnodes node@(Node{..}) = case children of
      [] -> HashSet.empty
      cs -> HashSet.unions $ HashSet.singleton node:map subnodes cs
    nodesOK = HashSet.unions $ map (subnodes . node) ok
    nodesKO = HashSet.difference
      (HashSet.unions $ map (subnodes . node) ko) nodesOK
    updateDataset :: Class -> HashSet Node -> Dataset -> Dataset
    updateDataset klass nodes dataset =
      HashSet.foldl' (\dataset node@Node {..} ->
        case rule of
          Just rule -> HashMap.insertWith (++) rule
            [(extractFeatures node, klass)] dataset
          Nothing -> dataset
        ) dataset nodes
    dataset' = updateDataset False nodesKO $ updateDataset True nodesOK dataset

-- | Build a dataset (rule name -> datums)
makeDataset :: [Rule] -> Corpus -> Dataset
makeDataset rules (context, options, examples) =
   List.foldl' (makeDataset1 rules context options) HashMap.empty examples
