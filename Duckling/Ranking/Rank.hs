-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Ranking.Rank
  ( rank
  ) where

import Control.Arrow ((***))
import Control.Monad (join)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Maybe
import qualified Data.Set as Set
import Prelude

import Duckling.Dimensions.Types
import Duckling.Ranking.Extraction
import Duckling.Ranking.Types
import Duckling.Types

classify :: Classifier -> BagOfFeatures -> (Class, Double)
classify Classifier {..} feats = if okScore >= koScore
  then (True, okScore)
  else (False, koScore)
  where
    (okScore, koScore) = join (***) (p feats) (okData, koData)
    p :: BagOfFeatures -> ClassData -> Double
    p feats ClassData{..} =
      prior + HashMap.foldrWithKey (\feat x res ->
        res + fromIntegral x * HashMap.lookupDefault unseen feat likelihoods
      ) 0.0 feats

score :: Classifiers -> Node -> Double
score classifiers node@Node {rule = Just rule, ..} =
  case HashMap.lookup rule classifiers of
    Just c -> let feats = extractFeatures node
      in snd (classify c feats) + sum (map (score classifiers) children)
    Nothing -> 0.0
score _ Node {rule = Nothing} = 0.0

-- | Return all superior candidates, as defined by the partial ordering
winners :: Ord a => [a] -> [a]
winners xs = filter (\x -> all ((/=) LT . compare x) xs) xs

-- | Return a curated list of tokens
rank
  :: Classifiers
  -> HashSet (Some Dimension)
  -> [ResolvedToken]
  -> [ResolvedToken]
rank classifiers targets tokens =
  Set.toList . Set.fromList
  . map (\(Candidate token _ _) -> token)
  . winners
  $ map makeCandidate tokens
  where
    makeCandidate :: ResolvedToken -> Candidate
    makeCandidate token@Resolved {node = n@Node {token = Token d _}} =
      Candidate token (score classifiers n) $ HashSet.member (This d) targets
