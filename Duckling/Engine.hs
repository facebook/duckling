-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Engine
  ( parseAndResolve
  , lookupRegex
  , runDuckling
  ) where

import Control.DeepSeq
import Control.Monad.Extra
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.Char as Char
import Data.Functor.Identity
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.List as L
import qualified Data.Vector.Primitive as Vector
import Prelude
import qualified Text.Regex.Base as R
import qualified Text.Regex.PCRE as PCRE

import Duckling.Dimensions.Types
import Duckling.Engine.Regex
import Duckling.Regex.Types
import Duckling.Resolve
import Duckling.Types
import qualified Duckling.Stash as Stash
import Duckling.Stash (Stash)

-- -----------------------------------------------------------------
-- Engine

type Duckling a = Identity a

runDuckling :: Duckling a -> a
runDuckling ma = runIdentity ma

parseAndResolve :: [Rule] -> Text -> Context -> [ResolvedToken]
parseAndResolve rules input context = mapMaybe (resolveNode context) .
  force $ Stash.toPosOrderedList $ runDuckling $
  parseString rules (mkDocument input) Stash.empty Stash.empty []

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

-- As regexes are matched without whitespace delimitator, we need to check
-- the reasonability of the match to actually be a word.
isRangeValid :: Document -> Range -> Bool
isRangeValid Document { indexable = s } (Range start end) =
  (start == 0 || isDifferent (s Vector.! (start - 1)) (s Vector.! start)) &&
  (end == Vector.length s ||
      isDifferent (s Vector.! (end - 1)) (s Vector.! end))
  where
    charClass :: Char -> Char
    charClass c
      | Char.isLower c = 'l'
      | Char.isUpper c = 'u'
      | Char.isDigit c = 'd'
      | otherwise = c
    isDifferent :: Char -> Char -> Bool
    isDifferent a b = charClass a /= charClass b

lookupRegex :: PCRE.Regex -> Int -> Document -> Duckling [Node]
lookupRegex regex position Document { rawInput = s } =
  return nodes
  where
    ss = Text.drop position s
    (nodes, _, _) = L.foldl' f ([], ss, position) $ match regex ss
    f (nodes, s, offset) [] = (reverse nodes, s, offset)
    f (nodes, s, offset) ("":_) = (reverse nodes, s, offset)
    f (nodes, s, offset) (text:group) = (node:nodes, s', newOffset)
      where
        (x,xs) = Text.breakOn text s
        m = offset + Text.length x
        n = Text.length text
        s' = Text.drop n xs
        newOffset = m + n
        node = Node
          { nodeRange = Range m newOffset
          , token = Token RegexMatch (GroupMatch group)
          , children = []
          , rule = Nothing
          }

lookupItem :: Document -> PatternItem -> Stash -> Int -> Duckling [Node]
lookupItem s (Regex re) _ position =
  filter (\node -> isRangeValid s (nodeRange node) &&
                   isPositionValid position s node) <$>
  lookupRegex re position s
lookupItem s (Predicate p) stash position =
  return $
  filter (p . token) $
  takeWhile (isPositionValid position s) $
  Stash.toPosOrderedListFrom stash position

isPositionValid :: Int -> Document -> Node -> Bool
isPositionValid position sentence (Node {nodeRange = Range start _}) =
  position == 0 || isAdjacent position start sentence

-- | A match is full if its rule pattern is empty.
-- (rule, endPosition, reversedRoute)
type Match = (Rule, Int, [Node])

-- | Recursively augments `matches`.
-- Discards partial matches stuck by a regex.
matchAll :: Document -> Stash -> [Match] -> Duckling [Match]
matchAll sentence stash matches = concatMapM mkNextMatches matches
  where
    mkNextMatches :: Match -> Duckling [Match]
    mkNextMatches match@(Rule {pattern = []}, _, _) = return [ match ]
    mkNextMatches match@(Rule {pattern = p:_}, _, _) = do
      nextMatches <- matchAll sentence stash =<< matchFirst sentence stash match
      return $ case p of
        Regex _ -> nextMatches
        Predicate _ -> match:nextMatches

-- | Returns all matches matching the first pattern item of `match`.
matchFirst :: Document -> Stash -> Match -> Duckling [Match]
matchFirst _ _ (Rule {pattern = []}, _, _) = return []
matchFirst sentence stash (rule@(Rule {pattern = p:ps}), position, route) =
  map (\node@Node {nodeRange = Range _ pos'} ->
    let newRoute = node:route
    in newRoute `seq` (newRule, pos', newRoute)
  ) <$> lookupItem sentence p stash position
  where
  newRule = rule { pattern = ps }

-- | Finds new matches resulting from newly added tokens.
-- Produces new tokens from full matches.
parseString1
  :: [Rule] -> Document -> Stash -> Stash -> [Match]
  -> Duckling (Stash, [Match])
parseString1 rules sentence stash new matches = do
  -- Recursively match patterns.
  -- Find which `matches` can advance because of `new`.
  newPartial <- concatMapM (matchFirst sentence new) matches

  -- Find new matches resulting from newly added tokens (`new`)
  -- For the first pass, pass through all rules.
  -- For subsequent passes, only try rules starting with a predicate.
  let match rule = matchFirst sentence new (rule, 0, [])
  newMatches <- if Stash.null stash
    then concatMapM match rules
    else concatMapM match
           [ rule | rule@(Rule {pattern = (Predicate _:_)}) <- rules ]

  (full, partial) <- L.partition (\(Rule {pattern}, _, _) -> null pattern)
    <$> matchAll sentence stash (newPartial ++ newMatches)

  -- Produce full matches as new tokens
  return ( Stash.fromList $ mapMaybe produce full
         , matches ++ partial
         )

-- | Produces all tokens recursively.
parseString :: [Rule] -> Document -> Stash -> Stash -> [Match] -> Duckling Stash
parseString rules sentence stash new matches = do
  (new', matches') <- parseString1 rules sentence stash new matches
  if Stash.null new'
    then return stash
    else parseString rules sentence (Stash.union stash new') new' matches'

resolveNode :: Context -> Node -> Maybe ResolvedToken
resolveNode context n@Node{token = (Token _ dd), nodeRange = nodeRange} = do
  val <- resolve context dd
  Just Resolved
    { range = nodeRange
    , node = n
    , jsonValue = toJSON val
    }
