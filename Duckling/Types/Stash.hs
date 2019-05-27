-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Types.Stash where

import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import qualified Data.HashSet as HashSet
import Data.HashSet (HashSet)
import Data.Maybe
import Prelude

import Duckling.Types

newtype Stash = Stash { getSet :: IntMap (HashSet Node) }

filter :: (Node -> Bool) -> Stash -> Stash
filter p Stash{..} = Stash (IntMap.map (HashSet.filter p) getSet)

toPosOrderedList:: Stash -> [Node]
toPosOrderedList Stash{..} = concatMap HashSet.toList $ IntMap.elems getSet

toPosOrderedListFrom :: Stash -> Int -> [Node]
toPosOrderedListFrom Stash{..} pos =
  concatMap HashSet.toList $ maybeToList equal ++ IntMap.elems bigger
  where
  (_smaller, equal, bigger) = IntMap.splitLookup pos getSet
  -- this is where we take advantage of the order

empty :: Stash
empty = Stash IntMap.empty

fromList :: [Node] -> Stash
fromList ns = Stash (IntMap.fromListWith HashSet.union $ map mkKV ns)
  where
  mkKV n@Node{nodeRange = Range start _} = (start, HashSet.singleton n)

union :: Stash -> Stash -> Stash
union (Stash set1) (Stash set2) =
  Stash (IntMap.unionWith HashSet.union set1 set2)

-- Checks if two stashes have equal amount of Nodes on each position.
-- Used to detect a fixpoint, because the Stashes are only growing.
--
-- Not proud of this, but the algorithm shouldn't use it as the termination
-- condition, it should know when it stopped adding tokens
sizeEqual :: Stash -> Stash -> Bool
sizeEqual (Stash set1) (Stash set2) =
  go (IntMap.toAscList set1) (IntMap.toAscList set2)
  where
  go [] [] = True
  go [] (_:_) = False
  go (_:_) [] = False
  go ((k1, h1):rest1) ((k2, h2):rest2) =
    k1 == k2 && HashSet.size h1 == HashSet.size h2 && go rest1 rest2

null :: Stash -> Bool
null (Stash set) = IntMap.null set
