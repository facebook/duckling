-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.HU.Rules
  ( rules ) where

import Data.HashMap.Strict (HashMap)
import Data.Maybe
import Data.String
import Data.Text (Text)
import Prelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Regex.Types
import Duckling.Types
import qualified Duckling.Numeral.Types as TNumeral

ruleIntegerNumeric :: Rule
ruleIntegerNumeric = Rule
  { name = "integer (numeric)"
  , pattern =
    [ regex "(\\d{1,18})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):
       _) -> do
         v <- parseInt match
         integer $ toInteger v
      _ -> Nothing
  }

ruleNumeralMap :: HashMap Text Integer
ruleNumeralMap = HashMap.fromList
  [ ( "nulla", 0 )
  , ( "z\x00E9r\x00F3", 0 )
  , ( "egy", 1 )
  , ( "kett\x0151", 2 )
  , ( "h\x00E1rom", 3 )
  , ( "n\x00E9gy", 4 )
  , ( "\x00F6t", 5)
  , ( "hat", 6)
  , ( "h\x00E9t", 7)
  , ( "nyolc", 8)
  , ( "kilenc", 9)
  , ( "t\x00EDz", 10)
  ]

ruleNumeral :: Rule
ruleNumeral = Rule
  { name = "number (0..10)"
  , pattern =
    [ regex "(nulla|z\x00E9r\x00F3|egy|kett\x0151|h\x00E1rom|n\x00E9gy|\x00F6t|hat|h\x00E9t|nyolc|kilenc|t\x00EDz)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) ruleNumeralMap >>= integer
      _ -> Nothing
  }

elevenToNineteenMap :: HashMap Text Integer
elevenToNineteenMap = HashMap.fromList
  [ ( "tizenegy", 11 )
  , ( "tizenkett\x0151", 12 )
  , ( "tizenh\x00E1rom", 13 )
  , ( "tizenn\x00E9gy", 14 )
  , ( "tizen\x00F6t", 15 )
  , ( "tizenhat", 16 )
  , ( "tizenh\x00E9t", 17 )
  , ( "tizennyolc", 18 )
  , ( "tizenkilenc", 19 )
  ]

ruleElevenToNineteen :: Rule
ruleElevenToNineteen = Rule
  { name = "number (11..19)"
  , pattern =
    [ regex "(tizenegy|tizenkett\x0151|tizenh\x00E1rom|tizenn\x00E9gy|tizen\x00F6t|tizenhat|tizenh\x00E9t|tizennyolc|tizenkilenc)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) elevenToNineteenMap >>= integer
      _ -> Nothing
  }

twentyoneToTwentynineMap :: HashMap Text Integer
twentyoneToTwentynineMap = HashMap.fromList
  [ ( "huszonegy", 21 )
  , ( "huszonkett\x0151", 22 )
  , ( "huszonh\x00E1rom", 23 )
  , ( "huszonn\x00E9gy", 24 )
  , ( "huszon\x00F6t", 25 )
  , ( "huszonhat", 26 )
  , ( "huszonh\x00E9t", 27 )
  , ( "huszonnyolc", 28 )
  , ( "huszonkilenc", 29 )
  ]

ruleTwentyoneToTwentynine :: Rule
ruleTwentyoneToTwentynine = Rule
  { name = "number (21..29)"
  , pattern =
    [ regex "(huszonegy|huszonkett\x0151|huszonh\x00E1rom|huszonn\x00E9gy|huszon\x00F6t|huszonhat|huszonh\x00E9t|huszonnyolc|huszonkilenc)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) twentyoneToTwentynineMap >>= integer
      _ -> Nothing
  }

ruleTens :: Rule
ruleTens = Rule
  { name = "integer (20,30..90)"
  , pattern = [regex "(h\x00FAsz|harminc|negyven|\x00F6tven|hatvan|hetven|nyolcvan|kilencven)"]
  , prod = \tokens ->
      case tokens of
        (Token RegexMatch (GroupMatch (match:_)):_) ->
          case Text.toLower match of
            "h\x00FAsz"  -> integer 20
            "harminc"    -> integer 30
            "negyven"    -> integer 40
            "\x00F6tven" -> integer 50
            "hatvan"     -> integer 60
            "hetven"     -> integer 70
            "nyolcvan"   -> integer 80
            "kilencven"  -> integer 90
            _            -> Nothing
        _ -> Nothing
  }

ruleCompositeTens :: Rule
ruleCompositeTens = Rule
  { name = "integer ([3-9][1-9])"
  , pattern =
    [ regex "(harminc|negyven|\x00F6tven|hatvan|hetven|nyolcvan|kilencven)(egy|kett\x0151|h\x00E1rom|n\x00E9gy|\x00F6t|hat|h\x00E9t|nyolc|kilenc)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:m2:_)):_) -> do
        v1 <- case Text.toLower m1 of
          "harminc"      -> Just 30
          "negyven"      -> Just 40
          "\x00F6tven"   -> Just 50
          "hatvan"       -> Just 60
          "hetven"       -> Just 70
          "nyolcvan"     -> Just 80
          "kilencven"    -> Just 90
          _ -> Nothing
        v2 <- case Text.toLower m2 of
          "egy"          -> Just 1 
          "kett\x0151"   -> Just 2 
          "h\x00E1rom"   -> Just 3 
          "n\x00E9gy"    -> Just 4 
          "\x00F6t"      -> Just 5
          "hat"          -> Just 6
          "h\x00E9t"     -> Just 7
          "nyolc"        -> Just 8
          "kilenc"       -> Just 9
          _ -> Nothing
        integer $ v1 + v2
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleIntegerNumeric
  , ruleNumeral
  , ruleElevenToNineteen
  , ruleTwentyoneToTwentynine
  , ruleTens
  , ruleCompositeTens
  ]
