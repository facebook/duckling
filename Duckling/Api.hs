-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}

module Duckling.Api
  ( analyze
  , formatToken
  , parse
  , supportedDimensions
  ) where

import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Text (Text)
import Prelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Dimensions
import Duckling.Engine
import Duckling.Locale
import Duckling.Ranking.Classifiers
import Duckling.Ranking.Rank
import Duckling.Resolve
import Duckling.Rules
import Duckling.Types

-- | Parses `input` and returns a curated list of entities found.
parse :: Text -> Context -> Options -> [Some Dimension] -> [Entity]
parse input ctx options = map (formatToken input) . analyze input ctx options .
  HashSet.fromList

supportedDimensions :: HashMap Lang [Some Dimension]
supportedDimensions =
  HashMap.fromList [ (l, allDimensions l) | l <- [minBound..maxBound] ]

-- | Returns a curated list of resolved tokens found
-- When `targets` is non-empty, returns only tokens of such dimensions.
analyze :: Text -> Context -> Options -> HashSet (Some Dimension)
  -> [ResolvedToken]
analyze input context@Context{..} options targets =
  rank (classifiers locale) targets
  . filter (\Resolved{node = Node{token = (Token d _)}} ->
      HashSet.null targets || HashSet.member (This d) targets
    )
  $ parseAndResolve (rulesFor locale targets) input context options

-- | Converts the resolved token to the API format
formatToken :: Text -> ResolvedToken -> Entity
formatToken sentence
  (Resolved (Range start end) node@Node{token = Token dimension _} value latent)
  = Entity (toName dimension) body value start end latent node
  where
    body = Text.drop start $ Text.take end sentence
