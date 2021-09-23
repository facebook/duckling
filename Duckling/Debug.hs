-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Debug
  ( allParses
  , debug
  , debugCustom
  , fullParses
  , ptree
  ) where

import Data.Maybe
import Data.Text (Text)
import Prelude
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Duckling.Api
import Duckling.Dimensions.Types
import Duckling.Engine
import Duckling.Locale
import Duckling.Resolve
import Duckling.Rules
import Duckling.Testing.Types
import Duckling.Types

-- -----------------------------------------------------------------
-- API

debug :: Locale -> Text -> [Seal Dimension] -> IO [Entity]
debug locale = debugCustom testContext {locale = locale} testOptions

allParses :: Locale -> Text -> [Seal Dimension] -> IO [Entity]
allParses l input targets = debugTokens input $ parses l input targets

fullParses :: Locale -> Text -> [Seal Dimension] -> IO [Entity]
fullParses l input targets =
  debugTokens
    input
    $ filter
      (\Resolved{range = Range start end} -> start == 0 && end == n)
      $ parses l input targets
  where
    n = Text.length input

debugCustom :: Context -> Options -> Text -> [Seal Dimension] -> IO [Entity]
debugCustom context options input targets =
  debugTokens
    input
    $ analyze input context options $ HashSet.fromList targets

ptree :: Text -> Entity -> IO ()
ptree input Entity {enode} = pnode input 0 enode

-- -----------------------------------------------------------------
-- Internals

parses :: Locale -> Text -> [Seal Dimension] -> [ResolvedToken]
parses l input targets =
  filter isRelevantDimension tokens
  where
    tokens = parseAndResolve rules input testContext {locale = l} testOptions
    rules = rulesFor l $ HashSet.fromList targets
    isRelevantDimension Resolved{node = Node{token = (Token d _)}} =
      case targets of
        [] -> True
        _ -> elem (Seal d) targets

debugTokens :: Text -> [ResolvedToken] -> IO [Entity]
debugTokens input tokens = do
  mapM_ (ptree input) entities
  return entities
  where entities = map (formatToken input) tokens

pnode :: Text -> Int -> Node -> IO ()
pnode input depth Node {children, rule, nodeRange = Range start end} = do
  Text.putStrLn out
  mapM_ (pnode input (depth + 1)) children
  where
    out = Text.concat [ Text.replicate depth "-- ", name, " (", body, ")" ]
    name = fromMaybe "regex" rule
    body = Text.drop start $ Text.take end input
