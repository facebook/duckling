-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Api
  ( analyze
  , formatToken
  , parse
  , supportedDimensions
  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Text (Text)
import qualified Data.Text as Text
import Prelude
import TextShow

import Duckling.Dimensions.Types
import Duckling.Dimensions
import Duckling.Engine
import Duckling.Lang
import Duckling.Ranking.Classifiers
import Duckling.Ranking.Rank
import Duckling.Resolve
import Duckling.Rules
import Duckling.Types

-- | Parses `input` and returns a curated list of entities found.
parse :: Text -> Context -> [Some Dimension] -> [Entity]
parse input ctx = map (formatToken input) . analyze input ctx . HashSet.fromList

supportedDimensions :: HashMap Lang [Some Dimension]
supportedDimensions =
  HashMap.fromList [ (l, allDimensions l) | l <- [minBound..maxBound] ]

-- | Returns a curated list of resolved tokens found
-- When `targets` is non-empty, returns only tokens of such dimensions.
analyze :: Text -> Context -> HashSet (Some Dimension) -> [ResolvedToken]
analyze input context@Context{..} targets =
  rank (classifiers lang) targets
  . filter (\(Resolved{node = Node{token = (Token d _)}}) ->
      HashSet.null targets || HashSet.member (This d) targets
    )
  $ parseAndResolve (rulesFor lang targets) input context

-- | Converts the resolved token to the API format
formatToken :: Text -> ResolvedToken -> Entity
formatToken sentence (Resolved (Range start end) (Node{token=Token dimension _}) value) =
  Entity (toName dimension) body value start end
  where
    body = Text.drop start $ Text.take end sentence
