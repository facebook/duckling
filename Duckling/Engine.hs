-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Engine
  ( parseAndResolve
  , lookupRegexAnywhere
  , runDuckling
  ) where

import Control.DeepSeq
import Control.Monad.Extra
import Data.ByteString (ByteString)
import Data.Functor.Identity
import Data.Maybe
import Data.Text (Text)
import Prelude
import qualified Data.Array as Array
import qualified Data.Foldable as Foldable
import qualified Data.List as L
import qualified Text.Regex.PCRE as PCRE

import Duckling.Dimensions.Types
import Duckling.Locale
import Duckling.Regex.Types
import Duckling.Resolve
import Duckling.Types hiding (regex)
import Duckling.Types.Document (Document)
import Duckling.Types.Stash (Stash)
import qualified Duckling.Engine.Regex as Regex
import qualified Duckling.Types.Document as Document
import qualified Duckling.Types.Stash as Stash

-- -----------------------------------------------------------------
-- Engine

parseAndResolve :: [Rule] -> Text -> Context -> Options -> [ResolvedToken]
parseAndResolve rules input context@Context{locale = Locale lang _} options =
  mapMaybe
    (resolveNode context options)
    $ force $ Stash.toPosOrderedList
      $ runDuckling $ parseString lang rules (Document.fromText input)

type Duckling a = Identity a

runDuckling :: Duckling a -> a
runDuckling ma = runIdentity ma

-- | A match is full if its rule pattern is empty.
-- (rule, endPosition, reversedRoute)
type Match = (Rule, Int, [Node])

resolveNode :: Context -> Options -> Node -> Maybe ResolvedToken
resolveNode context options n@Node{token = (Token dim dd), nodeRange = r}
  = do
  (val, latent) <- resolve context options dd
  Just Resolved
    { range = r
    , node = n
    , rval = RVal dim val
    , isLatent = latent
    }

parseString :: Lang -> [Rule] -> Document -> Duckling Stash
parseString lang rules sentence = do
  (new, partialMatches) <-
    -- One the first pass we try all the rules
    parseString1 lang rules sentence Stash.empty Stash.empty []
  if Stash.null new
    then return Stash.empty
    else
    -- For subsequent passes, we only try rules starting with a predicate.
    saturateParseString lang headPredicateRules sentence new new partialMatches
  where
  headPredicateRules =
    [ rule | rule@Rule{pattern = (Predicate _ : _)} <- rules ]

-- | Produces all tokens recursively.
saturateParseString
  :: Lang -> [Rule] -> Document -> Stash -> Stash -> [Match] -> Duckling Stash
saturateParseString lang rules sentence stash new matches = do
  (new', matches') <- parseString1 lang rules sentence stash new matches
  let stash' = Stash.union stash new'
  if Stash.null new'
    then return stash
    else saturateParseString lang rules sentence stash' new' matches'

-- | Finds new matches resulting from newly added tokens.
-- Produces new tokens from full matches.
parseString1
  :: Lang -> [Rule] -> Document -> Stash -> Stash -> [Match]
  -> Duckling (Stash, [Match])
parseString1 lang rules sentence stash new matches = do
  -- Recursively match patterns.
  -- Find which `matches` can advance because of `new`.
  newPartial <- concatMapM (matchFirst sentence lang new) matches

  -- Find new matches resulting from newly added tokens (`new`)
  newMatches <- concatMapM (matchFirstAnywhere sentence lang new) rules

  (full, partial) <- L.partition (\(Rule {pattern}, _, _) -> null pattern)
    <$> matchAll sentence lang stash (newPartial ++ newMatches)

  -- Produce full matches as new tokens
  return ( Stash.fromList $ mapMaybe produce full
         , partial ++ matches
         )

-- | Recursively augments `matches`.
-- Discards partial matches stuck by a regex.
matchAll :: Document -> Lang -> Stash -> [Match] -> Duckling [Match]
matchAll sentence lang stash matches = concatMapM mkNextMatches matches
  where
    mkNextMatches :: Match -> Duckling [Match]
    mkNextMatches match@(Rule {pattern = []}, _, _) = return [ match ]
    mkNextMatches match@(Rule {pattern = p:_}, _, _) = do
      nextMatches <- matchAll sentence lang stash =<< matchFirst sentence lang stash match
      return $ case p of
        Regex _ -> nextMatches
        Predicate _ -> match:nextMatches

produce :: Match -> Maybe Node
produce (_, _, []) = Nothing
produce (Rule name _ production, _, etuor@(Node {nodeRange = Range _ e}:_)) = do
  let route = reverse etuor
  token <- force $ production $ map token route
  case route of
    (Node {nodeRange = Range p _}:_) -> Just Node
      { nodeRange = Range p e
      , token = token
      , children = route
      , rule = Just name
      }
    [] -> Nothing

-- | Returns all matches matching the first pattern item of `match`,
-- resuming from a Match position
matchFirst :: Document -> Lang -> Stash -> Match -> Duckling [Match]
matchFirst _ _ _ (Rule {pattern = []}, _, _) = return []
matchFirst sentence lang stash (rule@Rule{pattern = p : ps}, position, route) =
  map (mkMatch route newRule) <$> lookupItem sentence lang p stash position
  where
  newRule = rule { pattern = ps }

-- | Returns all matches matching the first pattern item of `match`,
-- starting anywhere
matchFirstAnywhere :: Document -> Lang -> Stash -> Rule -> Duckling [Match]
matchFirstAnywhere _sentence _lang _stash Rule {pattern = []} = return []
matchFirstAnywhere sentence lang stash rule@Rule{pattern = p : ps} =
  map (mkMatch [] newRule) <$> lookupItemAnywhere sentence lang p stash
  where
  newRule = rule { pattern = ps }

-- | Handle one PatternItem at a given position
lookupItem :: Document -> Lang -> PatternItem -> Stash -> Int -> Duckling [Node]
lookupItem doc lang (Regex re) _ position =
  filter (isPositionValid position doc) <$>
  lookupRegex doc lang re position
lookupItem doc _lang (Predicate p) stash position =
  return $
  filter (p . token) $
  takeWhile (isPositionValid position doc) $
  Stash.toPosOrderedListFrom stash position

-- | Handle one PatternItem anywhere in the text
lookupItemAnywhere :: Document -> Lang -> PatternItem -> Stash -> Duckling [Node]
lookupItemAnywhere doc lang (Regex re) _ = lookupRegexAnywhere doc lang re
lookupItemAnywhere _doc _lang (Predicate p) stash =
  return $ filter (p . token) $ Stash.toPosOrderedList stash

isPositionValid :: Int -> Document -> Node -> Bool
isPositionValid position sentence Node{nodeRange = Range start _} =
  Document.isAdjacent sentence position start

{-# INLINE mkMatch #-}
mkMatch :: [Node] -> Rule -> Node -> Match
mkMatch route newRule node@Node{nodeRange = Range _ pos'} =
  newRoute `seq` (newRule, pos', newRoute)
  where newRoute = node:route

-- | Handle a regex match at a given position
lookupRegex :: Document -> Lang -> PCRE.Regex -> Int -> Duckling [Node]
lookupRegex doc _lang _regex position | position >= Document.length doc = return []
lookupRegex doc lang regex position =
  lookupRegexCommon doc lang regex position Regex.matchOnce

-- | Handle a regex match anywhere in the text
lookupRegexAnywhere :: Document -> Lang -> PCRE.Regex -> Duckling [Node]
lookupRegexAnywhere doc lang regex = lookupRegexCommon doc lang regex 0 Regex.matchAll

{-# INLINE lookupRegexCommon #-}
-- INLINE bloats the code a bit, but the code is better
lookupRegexCommon
  :: Foldable t
  => Document
  -> Lang
  -> PCRE.Regex
  -> Int
  -> (PCRE.Regex -> ByteString -> t PCRE.MatchArray)
  -> Duckling [Node]
lookupRegexCommon doc lang regex position matchFun = return nodes
  where
  -- See Note [Regular expressions and Text] from Document.hs to understand
  -- what's going on here
  (substring, rangeToText, translateRange) =
    Document.byteStringFromPos doc position
  nodes = mapMaybe (f . Array.elems)
    $ Foldable.toList
    $ matchFun regex substring
  f :: [(Int, Int)] -> Maybe Node
  f [] = Nothing
  f ((0,0):_) = Nothing
  f ((bsStart, bsLen):groups) =
    if Document.isRangeValid lang doc start end
      then Just node
      else Nothing
    where
    textGroups = map rangeToText groups
    (start, end) = translateRange bsStart bsLen
    node = Node
      { nodeRange = Range start end
      , token = Token RegexMatch (GroupMatch textGroups)
      , children = []
      , rule = Nothing
      }
