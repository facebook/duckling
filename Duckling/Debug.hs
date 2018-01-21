-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Debug
  ( allParses
  , debug
  , debugContext
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

debug :: Locale -> Text -> [Some Dimension] -> IO [Entity]
debug locale = debugContext testContext {locale = locale}

allParses :: Locale -> Text -> [Some Dimension] -> IO [Entity]
allParses l sentence targets = debugTokens sentence $ parses l sentence targets

fullParses :: Locale -> Text -> [Some Dimension] -> IO [Entity]
fullParses l sentence targets = debugTokens sentence .
  filter (\(Resolved {range = Range start end}) -> start == 0 && end == n) $
  parses l sentence targets
  where
    n = Text.length sentence

ptree :: Text -> ResolvedToken -> IO ()
ptree sentence Resolved {node} = pnode sentence 0 node

-- -----------------------------------------------------------------
-- Internals

parses :: Locale -> Text -> [Some Dimension] -> [ResolvedToken]
parses l sentence targets = flip filter tokens $
  \(Resolved {node = Node{token = (Token d _)}}) ->
    case targets of
      [] -> True
      _ -> elem (This d) targets
  where
    tokens = parseAndResolve rules sentence testContext {locale = l}
    rules = rulesFor l $ HashSet.fromList targets

debugContext :: Context -> Text -> [Some Dimension] -> IO [Entity]
debugContext context sentence targets =
  debugTokens sentence . analyze sentence context $ HashSet.fromList targets

debugTokens :: Text -> [ResolvedToken] -> IO [Entity]
debugTokens sentence tokens = do
  mapM_ (ptree sentence) tokens
  return $ map (formatToken sentence) tokens

pnode :: Text -> Int -> Node -> IO ()
pnode sentence depth Node {children, rule, nodeRange = Range start end} = do
  Text.putStrLn out
  mapM_ (pnode sentence (depth + 1)) children
  where
    out = Text.concat [ Text.replicate depth "-- ", name, " (", body, ")" ]
    name = fromMaybe "regex" rule
    body = Text.drop start $ Text.take end sentence
