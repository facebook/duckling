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
{-# LANGUAGE BangPatterns #-}

module Duckling.Engine
  ( parseAndResolve
  , lookupRegex
  , runDuckling
  ) where

import Control.DeepSeq
import Control.Monad.Extra
import Data.Aeson
import qualified Data.Array as Array
import qualified Data.Array.Unboxed as UArray
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Char as Char
import Data.Functor.Identity
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.Unsafe as UText
import qualified Data.List as L
import Prelude
import qualified Text.Regex.PCRE as PCRE

import Duckling.Dimensions.Types
import qualified Duckling.Engine.Regex as Regex
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
  parseString rules (mkDocument input)

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
  (start == 0 || isDifferent (s UArray.! (start - 1)) (s UArray.! start)) &&
  (end == arraySize s ||
      isDifferent (s UArray.! (end - 1)) (s UArray.! end))
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
lookupRegex _regex position Document{ indexable = indexable }
  | position >= arraySize indexable = return []
lookupRegex regex position
  Document { rawInput = rawInput
           , utf8Encoded = utf8Encoded
           , tDropToBSDrop = tDropToBSDrop
           , bsDropToTDrop = bsDropToTDrop
           , tDropToUtf16Drop = tDropToUtf16Drop
           } = return nodes
  where
    -- See Note [Regular expressions and Text] to understand what's going
    -- on here
    utf8Position = tDropToBSDrop UArray.! position
    substring :: ByteString
    substring = BS.drop utf8Position utf8Encoded
    nodes = L.foldl' f [] $ map Array.elems $ Regex.matchAll regex substring
    f :: [Node] -> [(Int, Int)] -> [Node]
    f nodes [] = nodes
    f nodes ((0,0):_) = nodes
    f nodes ((bsStart, bsLen):groups) = node:nodes
      where
      textGroups = map rangeToText groups
      node = Node
        { nodeRange = uncurry Range $ translateRange bsStart bsLen
        , token = Token RegexMatch (GroupMatch textGroups)
        , children = []
        , rule = Nothing
        }
    -- get a subrange of Text reusing the underlying buffer using
    -- utf16 start and end positions
    rangeToText :: (Int, Int) -> Text
    rangeToText (-1, _) = ""
    -- this is what regexec from Text.Regex.PCRE.ByteString does
    rangeToText r = UText.takeWord16 (end16Pos - start16Pos) $
      UText.dropWord16 start16Pos rawInput
      where
      start16Pos = tDropToUtf16Drop UArray.! startPos
      end16Pos = tDropToUtf16Drop UArray.! endPos
      (startPos, endPos) = uncurry translateRange r
    -- from utf8 offset and length to Text character start and end position
    translateRange :: Int -> Int -> (Int, Int)
    translateRange !bsStart !bsLen = startPos `seq` endPos `seq` res
      where
      res = (startPos, endPos)
      realBsStart = utf8Position + bsStart
      realBsEnd = realBsStart + bsLen
      startPos = bsDropToTDrop UArray.! realBsStart
      endPos = bsDropToTDrop UArray.! realBsEnd

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
  let match rule = matchFirst sentence new (rule, 0, [])
  newMatches <- concatMapM match rules

  (full, partial) <- L.partition (\(Rule {pattern}, _, _) -> null pattern)
    <$> matchAll sentence stash (newPartial ++ newMatches)

  -- Produce full matches as new tokens
  return ( Stash.fromList $ mapMaybe produce full
         , partial ++ matches
         )

-- | Produces all tokens recursively.
saturateParseString
  :: [Rule] -> Document -> Stash -> Stash -> [Match] -> Duckling Stash
saturateParseString rules sentence stash new matches = do
  (new', matches') <- parseString1 rules sentence stash new matches
  let stash' = Stash.union stash new'
  if Stash.null new'
    then return stash
    else saturateParseString rules sentence stash' new' matches'

parseString :: [Rule] -> Document -> Duckling Stash
parseString rules sentence = do
  (new, partialMatches) <-
    -- One the first pass we try all the rules
    parseString1 rules sentence Stash.empty Stash.empty []
  if Stash.null new
    then return Stash.empty
    else
    -- For subsequent passes, we only try rules starting with a predicate.
    saturateParseString headPredicateRules sentence new new partialMatches
  where
  headPredicateRules =
    [ rule | rule@(Rule {pattern = (Predicate _:_)}) <- rules ]

resolveNode :: Context -> Node -> Maybe ResolvedToken
resolveNode context n@Node{token = (Token _ dd), nodeRange = nodeRange} = do
  val <- resolve context dd
  Just Resolved
    { range = nodeRange
    , node = n
    , jsonValue = toJSON val
    }
