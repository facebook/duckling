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
  ) where

import Control.DeepSeq
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.Char as Char
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.List as L
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

parseAndResolve :: [Rule] -> Text -> Context -> [ResolvedToken]
parseAndResolve rules input context = mapMaybe (resolveNode context) .
  force $ Stash.toPosOrderedList $
  parseString rules input Stash.empty Stash.empty []

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

-- True iff a is followed by whitespaces and b.
isAdjacent :: Int -> Int -> Text -> Bool
isAdjacent a b s = b >= a && (Text.all isAdjacentSeparator . substr s a $ b - a)
  where
    isAdjacentSeparator :: Char -> Bool
    isAdjacentSeparator c = elem c [' ', '\t', '-']

    substr :: Text  -- ^ String
           -> Int   -- ^ Offset
           -> Int   -- ^ Length
           -> Text  -- ^ Substring
    substr txt start (-1)  = Text.drop start txt
    substr txt start count = Text.take count $ Text.drop start txt

-- As regexes are matched without whitespace delimitator, we need to check
-- the reasonability of the match to actually be a word.
isRangeValid :: Text -> Range -> Bool
isRangeValid s (Range start end) =
  (start == 0 || isDifferent (Text.index s (start - 1)) (Text.index s start)) &&
  (end == Text.length s || isDifferent (Text.index s (end - 1)) (Text.index s end))
  where
    charClass :: Char -> Char
    charClass c
      | Char.isLower c = 'l'
      | Char.isUpper c = 'u'
      | Char.isDigit c = 'd'
      | otherwise = c
    isDifferent :: Char -> Char -> Bool
    isDifferent a b = charClass a /= charClass b

-- TODO(jodent) use Attoparsec t12879001
lookupRegex :: PCRE.Regex -> Int -> Text -> [Node]
lookupRegex regex position s = nodes
  where
    ss = Text.drop position s
    (nodes, _, _) = L.foldl' f ([], ss, position) $ match regex ss
    f res [] = res
    f res ("":_) = res
    f (nodes, s, offset) (text:group) = (nodes ++ [node], s', newOffset)
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

lookupItem :: Text -> PatternItem -> Stash -> Int -> [Node]
lookupItem s (Regex re) _ position =
  filter (\node -> isRangeValid s (nodeRange node) &&
                   isPositionValid position s node) $
  lookupRegex re position s
lookupItem s (Predicate p) stash position =
  filter (p . token) $
  takeWhile (isPositionValid position s) $
  Stash.toPosOrderedListFrom stash position

isPositionValid :: Int -> Text -> Node -> Bool
isPositionValid position sentence (Node {nodeRange = Range start _}) =
  position == 0 || isAdjacent position start sentence

-- | A match is full if its rule pattern is empty.
-- (rule, endPosition, reversedRoute)
type Match = (Rule, Int, [Node])

-- | Recursively augments `matches`.
-- Discards partial matches stuck by a regex.
matchAll :: Text -> Stash -> [Match] -> [Match]
matchAll sentence stash matches = concatMap mkNextMatches matches
  where
    mkNextMatches :: Match -> [Match]
    mkNextMatches match@(Rule {pattern = []}, _, _) = [ match ]
    mkNextMatches match@(Rule {pattern = p:_}, _, _) =
      let nextMatches = matchAll sentence stash $ matchFirst sentence stash match
      in case p of
        Regex _ -> nextMatches
        Predicate _ -> match:nextMatches

-- | Returns all matches matching the first pattern item of `match`.
matchFirst :: Text -> Stash -> Match -> [Match]
matchFirst _ _ (Rule {pattern = []}, _, _) = []
matchFirst sentence stash (rule@(Rule {pattern = p:ps}), position, route) =
  map (\node@Node {nodeRange = Range _ pos'} ->
    (rule {pattern = ps}, pos', node:route)
  ) $ lookupItem sentence p stash position

-- | Finds new matches resulting from newly added tokens.
-- Produces new tokens from full matches.
-- TODO(jodent) partially order matches and find longest whitespace sequence
parseString1 :: [Rule] -> Text -> Stash -> Stash -> [Match] -> (Stash, [Match])
parseString1 rules sentence stash new matches =
  -- Produce full matches as new tokens
  ( Stash.fromList $ mapMaybe produce full
  , matches ++ partial
  )
  where
    -- Recursively match patterns.
    (full, partial) = L.partition (\(Rule {pattern}, _, _) -> null pattern)
      . matchAll sentence stash
      $ newPartial ++ newMatches

    -- Find which `matches` can advance because of `new`.
    newPartial = concatMap (matchFirst sentence new) matches

    -- Find new matches resulting from newly added tokens (`new`)
    -- For the first pass, pass through all rules.
    -- For subsequent passes, only try rules starting with a predicate.
    newMatches = if Stash.null stash
      then [ x | rule <- rules, x <- matchFirst sentence new (rule, 0, []) ]
      else [ x
           | rule@(Rule {pattern = (Predicate _:_)}) <- rules
           , x <- matchFirst sentence new (rule, 0, [])
           ]

-- | Produces all tokens recursively.
parseString :: [Rule] -> Text -> Stash -> Stash -> [Match] -> Stash
parseString rules sentence stash new matches
  | Stash.null new' = stash
  | otherwise = parseString rules sentence stash' new' matches'
  where
    stash' = Stash.union stash new'
    (new', matches') = parseString1 rules sentence stash new matches

resolveNode :: Context -> Node -> Maybe ResolvedToken
resolveNode context n@Node{token = (Token _ dd), nodeRange = nodeRange} = do
  val <- resolve context dd
  Just Resolved
    { range = nodeRange
    , node = n
    , jsonValue = toJSON val
    }
