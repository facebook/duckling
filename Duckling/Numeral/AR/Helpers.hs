-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Duckling.Numeral.AR.Helpers
  ( digitsMap
  , numeralToStringMap
  , parseArabicDoubleAsText
  , parseArabicDoubleFromText
  , parseArabicIntAsText
  , parseArabicIntegerFromText
  ) where

import Control.Monad (join)
import Data.HashMap.Strict (HashMap)
import Data.Maybe (mapMaybe)
import Data.String
import Data.Text (Text)
import Duckling.Numeral.Helpers
  ( parseDouble
  , parseInteger
  )
import Prelude

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

numeralToStringMap :: HashMap Char String
numeralToStringMap =
  HashMap.fromList
    [ ('٠', "0")
    , ('١', "1")
    , ('٢', "2")
    , ('٣', "3")
    , ('٤', "4")
    , ('٥', "5")
    , ('٦', "6")
    , ('٧', "7")
    , ('٨', "8")
    , ('٩', "9")
    ]

digitsMap :: HashMap Text Integer
digitsMap =
  HashMap.fromList
    [ ("عشر", 2)
    , ("ثلاث", 3)
    , ("اربع", 4)
    , ("أربع", 4)
    , ("خمس", 5)
    , ("ست", 6)
    , ("سبع", 7)
    , ("ثمان", 8)
    , ("تسع", 9)
    ]

parseArabicIntAsText :: Text -> Text
parseArabicIntAsText =
  Text.pack
    . join
    . mapMaybe (`HashMap.lookup` numeralToStringMap)
    . Text.unpack

parseArabicIntegerFromText :: Text -> Maybe Integer
parseArabicIntegerFromText = parseInteger . parseArabicIntAsText

parseArabicDoubleAsText :: Text -> Text
parseArabicDoubleAsText =
  Text.pack
    . join
    . mapMaybe (`HashMap.lookup` HashMap.insert '٫' "." numeralToStringMap)
    . Text.unpack

parseArabicDoubleFromText :: Text -> Maybe Double
parseArabicDoubleFromText = parseDouble . parseArabicDoubleAsText
