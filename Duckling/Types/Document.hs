-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Duckling.Types.Document
  ( Document -- abstract
  , fromText
  , (!)
  , length
  , byteStringFromPos
  , isAdjacent
  , isRangeValid
  ) where

import Data.Array.Unboxed (UArray)
import Data.ByteString (ByteString)
import Data.List (scanl', foldl', foldr)
import Data.String
import Data.Text (Text)
import Prelude hiding (length)
import qualified Data.Array.Unboxed as Array
import qualified Data.ByteString as BS
import qualified Data.Char as Char
import qualified Data.Text.Unsafe as UText
import qualified Data.Text.Encoding as Text
import qualified Data.Text as Text
import qualified Data.Text.Internal.Unsafe.Char as UText


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
  fromString = fromText . fromString

fromText :: Text -> Document
fromText rawInput = Document{..}
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

-- As regexes are matched without whitespace delimitator, we need to check
-- the reasonability of the match to actually be a word.
isRangeValid :: Document -> Int -> Int -> Bool
isRangeValid doc start end =
  (start == 0 ||
      isDifferent (doc ! (start - 1)) (doc ! start)) &&
  (end == length doc ||
      isDifferent (doc ! (end - 1)) (doc ! end))
  where
    charClass :: Char -> Char
    charClass c
      | Char.isLower c || Char.isUpper c = 'c'
      | Char.isDigit c = 'd'
      | otherwise = c
    isDifferent :: Char -> Char -> Bool
    isDifferent a b = charClass a /= charClass b

-- True iff a is followed by whitespaces and b.
isAdjacent :: Document -> Int -> Int -> Bool
isAdjacent Document{..} a b =
  b >= a && (firstNonAdjacent Array.! a >= b)

isAdjacentSeparator :: Char -> Bool
isAdjacentSeparator c = elem c [' ', '\t']

(!) :: Document -> Int -> Char
(!) Document { indexable = s } ix = s Array.! ix

length :: Document -> Int
length Document { indexable = s } = Array.rangeSize $ Array.bounds s

-- | Given a document and an offset (think Text.drop offset),
-- returns a utf8 encoded substring of Document at that offset
-- and 2 translation functions:
--   rangeToText - given a range in the returned ByteString, gives
--     a corresponding subrange of the Document as Text
--   translateRange - given a start and a length of a range in the returned
--     ByteString, gives a corresponding subrange in the Document as pair
--     of (start, end) of Text.drop offsets
{-# INLINE byteStringFromPos #-}
-- if we don't inline we seem to pay for the tuple, there might be
-- an easier way
byteStringFromPos
  :: Document
  -> Int
  -> ( ByteString
     , (Int, Int) -> Text
     , Int -> Int -> (Int, Int)
     )
byteStringFromPos
  Document { rawInput = rawInput
           , utf8Encoded = utf8Encoded
           , tDropToBSDrop = tDropToBSDrop
           , bsDropToTDrop = bsDropToTDrop
           , tDropToUtf16Drop = tDropToUtf16Drop
           }
  position = (substring, rangeToText, translateRange)
  where
  -- See Note [Regular expressions and Text] to understand what's going
  -- on here
  utf8Position = tDropToBSDrop Array.! position
  substring :: ByteString
  substring = BS.drop utf8Position utf8Encoded
  -- get a subrange of Text reusing the underlying buffer using
  -- utf16 start and end positions
  rangeToText :: (Int, Int) -> Text
  rangeToText (-1, _) = ""
  -- this is what regexec from Text.Regex.PCRE.ByteString does
  rangeToText r = UText.takeWord16 (end16Pos - start16Pos) $
    UText.dropWord16 start16Pos rawInput
    where
    start16Pos = tDropToUtf16Drop Array.! startPos
    end16Pos = tDropToUtf16Drop Array.! endPos
    (startPos, endPos) = uncurry translateRange r
  -- from utf8 offset and length to Text character start and end position
  translateRange :: Int -> Int -> (Int, Int)
  translateRange !bsStart !bsLen = startPos `seq` endPos `seq` res
    where
    res = (startPos, endPos)
    realBsStart = utf8Position + bsStart
    realBsEnd = realBsStart + bsLen
    startPos = bsDropToTDrop Array.! realBsStart
    endPos = bsDropToTDrop Array.! realBsEnd
