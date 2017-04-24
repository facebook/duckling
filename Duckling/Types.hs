-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module Duckling.Types where

import Control.DeepSeq
import qualified Data.Array.Unboxed as Array
import Data.Array.Unboxed (UArray, IArray)
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LB
import Data.GADT.Compare
import Data.Hashable
import Data.Maybe
import Data.String
import Data.Text (Text)
import Data.List (scanl', foldl', foldr)
import qualified Data.Text.Encoding as Text
import qualified Data.Text as Text
import qualified Data.Text.Internal.Unsafe.Char as UText
import Data.Typeable ((:~:)(Refl), Typeable)
import GHC.Generics
import Prelude
import qualified Text.Regex.Base as R
import qualified Text.Regex.PCRE as PCRE

import Duckling.Dimensions.Types
import Duckling.Resolve

data Document = Document
  { rawInput :: !Text
  , utf8Encoded :: ByteString
  , indexable :: UArray Int Char -- for O(1) indexing pos -> Char
  , firstNonAdjacent :: UArray Int Int
    -- for a given index 'i' it keeps a first index 'j' greater or equal 'i'
    -- such that isAdjacentSeparator (indexable ! j) == False
    -- eg. " a document " :: Document
    --     firstNonAdjacent = [1,1,3,3,4,5,6,7,8,9,10,12]
    -- Note that in this case 12 is the length of the vector, hence not a
    -- valid index inside the array, this is intentional.
  , tDropToBSDrop :: UArray Int Int
    -- how many bytes to BS.drop from a utf8 encoded ByteString to
    -- reach the same position as Text.drop would
  , bsDropToTDrop :: UArray Int Int
    -- the inverse of tDropToBSDrop, rounds down for bytes that are
    -- not on character boundary
    -- for "żółty" :: Document
    --   tDropToBSDrop = [0,2,4,6,7,8]
    --   bsDropToTDrop = [0,1,1,2,2,3,3,4,5]
    --   tDropToUtf16Drop = [0,1,2,3,4,5]
  , tDropToUtf16Drop :: UArray Int Int
    -- translate Text.drop to Data.Text.Unsafe.dropWord16
  } deriving (Show)

{-
Note [Regular expressions and Text]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Text is UTF-16 encoded internally and PCRE operates on UTF-8 encoded
ByteStrings. Because we do a lot of regexp matching on the same Text,
it pays off to cache UTF-8 the encoded ByteString. That's the utf8Ecoded
field in Document.

Moreover we do regexp matching with capture, where the captured groups
are returned as ByteString and we want them as Text. But all of the
captured groups are just a substrings of the original Text.
Fortunately PCRE has an API that returns a MatchArray - a structure with
just the ByteString indices and ByteString lengths of the matched fragments.
If we play with indices right we can translate them to offsets into the
original Text, share the underlying Text buffer and avoid all of
UTF-8 and UTF-16 encoding and new ByteString and Text allocation.
-}

instance IsString Document where
  fromString = mkDocument . fromString

mkDocument :: Text -> Document
mkDocument rawInput = Document{..}
  where
  utf8Encoded = Text.encodeUtf8 rawInput
  rawInputLength = Text.length rawInput
  unpacked = Text.unpack rawInput
  indexable = Array.listArray (0, rawInputLength - 1) unpacked
  firstNonAdjacent = Array.listArray (0, rawInputLength - 1) $ snd $
   foldr gen (rawInputLength, []) $ zip [0..] unpacked
  -- go from the end keeping track of the first nonAdjacent (best)
  gen (ix, elem) (best, !acc)
    | isAdjacentSeparator elem = (best, best:acc)
    | otherwise = (ix, ix:acc)
  tDropToBSDropList = scanl' (\acc a -> acc + utf8CharWidth a) 0 unpacked
  tDropToBSDrop = Array.listArray (0, rawInputLength) tDropToBSDropList
  tDropToUtf16Drop = Array.listArray (0, rawInputLength) $
    scanl' (\acc a -> acc + utf16CharWidth a) 0 unpacked
  bsDropToTDrop = Array.listArray (0, BS.length utf8Encoded) $
    reverse $ snd $ foldl' fun (-1, []) $ zip [0..] tDropToBSDropList
  fun (lastPos, !acc) (ix, elem) = (elem, replicate (elem - lastPos) ix ++ acc)
  utf8CharWidth c
    | w <= 0x7F = 1
    | w <= 0x7FF = 2
    | w <= 0xFFFF = 3
    | otherwise = 4
    where
    w = UText.ord c
  utf16CharWidth c
    | w < 0x10000 = 1
    | otherwise = 2
    where
    w = UText.ord c


-- True iff a is followed by whitespaces and b.
isAdjacent :: Int -> Int -> Document -> Bool
isAdjacent a b Document{..} =
  b >= a && (firstNonAdjacent Array.! a >= b)

isAdjacentSeparator :: Char -> Bool
isAdjacentSeparator c = elem c [' ', '\t', '-']

arraySize :: IArray UArray a => UArray Int a -> Int
arraySize = Array.rangeSize . Array.bounds

-- -----------------------------------------------------------------
-- Token

data Token = forall a . (Resolve a, Eq a, Hashable a, Show a, NFData a) =>
  Token (Dimension a) a

deriving instance Show Token
instance Eq Token where
  Token d1 v1 == Token d2 v2 = case geq d1 d2 of
    Just Refl -> v1 == v2
    Nothing   -> False

instance Hashable Token where
  hashWithSalt s (Token dim v) = hashWithSalt s (dim, v)

instance NFData Token where
  rnf (Token _ v) = rnf v

isDimension :: Dimension a -> Token -> Bool
isDimension dim (Token dim' _) = isJust $ geq dim dim'

data Node = Node
  { nodeRange :: Range
  , token     :: Token
  , children  :: [Node]
  , rule      :: Maybe Text
  } deriving (Eq, Generic, Hashable, Show, NFData)

data ResolvedToken = Resolved
  { range :: Range
  , node :: Node
  , jsonValue :: Value
  } deriving (Eq, Show)

instance Ord ResolvedToken where
  compare (Resolved range1 _ json1) (Resolved range2 _ json2) =
    case compare range1 range2 of
      EQ -> compare (toJText json1) (toJText json2)
      z  -> z

data Candidate = Candidate ResolvedToken Double Bool
  deriving (Eq, Show)

instance Ord Candidate where
  compare (Candidate (Resolved{range = Range s1 e1, node = Node{token = Token d1 _}}) score1 t1)
          (Candidate (Resolved{range = Range s2 e2, node = Node{token = tok2}}) score2 t2)
    | isDimension d1 tok2 = case starts of
        EQ -> case ends of
          EQ -> compare score1 score2
          z -> z
        LT -> case ends of
          LT -> EQ
          _ -> GT
        GT -> case ends of
          GT -> EQ
          _ -> LT
    | t1 == t2 = compRange
    | t1 && compRange == GT = GT
    | t2 && compRange == LT = LT
    | otherwise = EQ
      where
        starts = compare s1 s2
        ends = compare e1 e2
        -- a > b if a recovers b
        compRange = case starts of
          EQ -> ends
          LT -> case ends of
            LT -> EQ
            _  -> GT
          GT -> case ends of
            GT -> EQ
            _  -> LT

data Range = Range Int Int
  deriving (Eq, Ord, Generic, Hashable, Show, NFData)

type Production = [Token] -> Maybe Token
type Predicate = Token -> Bool
data PatternItem = Regex PCRE.Regex | Predicate Predicate

type Pattern = [PatternItem]

data Rule = Rule
  { name :: Text
  , pattern :: Pattern
  , prod :: Production
  }

instance Show Rule where
  show (Rule name _ _) = show name

data Entity = Entity
  { dim   :: Text
  , body  :: Text
  , value :: Text
  , start :: Int
  , end   :: Int
  } deriving (Eq, Generic, Show, NFData)

instance ToJSON Entity where
  toEncoding = genericToEncoding defaultOptions

toJText :: ToJSON x => x -> Text
toJText j = Text.decodeUtf8 $ LB.toStrict $ encode j

-- -----------------------------------------------------------------
-- Predicates helpers

regex :: String -> PatternItem
regex = Regex . R.makeRegexOpts compOpts execOpts
  where
    compOpts = PCRE.defaultCompOpt + PCRE.compCaseless
    execOpts = PCRE.defaultExecOpt

dimension :: Typeable a => Dimension a -> PatternItem
dimension value = Predicate $ isDimension value
