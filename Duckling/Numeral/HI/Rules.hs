-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.



{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Numeral.HI.Rules
  ( rules
  ) where

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
  [ ( "शून्य", 0 )
  , ( "०", 0 )
  , ( "एक", 1 )
  , ( "१", 1 )
  , ( "दो", 2 )
  , ( "२", 2 )
  , ( "तीन", 3 )
  , ( "३", 3 )
  , ( "चार", 4 )
  , ( "४", 4 )
  , ( "पाँच",5 )
  , ( "५",5 )
  , ( "छह", 6 )
  , ( "६", 6 )
  , ( "सात", 7 )
  , ( "७", 7 )
  , ( "आठ", 8 )
  , ( "८", 8 )
  , ("नौ", 9 )
  , ("९", 9 )
  , ("दस", 10 )
  , ("१०", 10 )
  ]

ruleNumeral :: Rule
ruleNumeral = Rule
  { name = "number (0..10)"
  , pattern =
    [ regex "(शून्य|०|एक|१|दो|२|तीन|३|चार|४|पाँच|५|छह|६|सात|७|आठ|८|नौ|९|दस|१०)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) ruleNumeralMap >>= integer
      _ -> Nothing
  }

elevenToNineteenMap :: HashMap Text Integer
elevenToNineteenMap = HashMap.fromList
  [ ( "ग्यारह", 11 )
  , ( "११", 11 )
  , ( "बारह", 12 )
  , ( "१२", 12 )
  , ( "तेरह", 13 )
  , ( "१३", 13 )
  , ( "चौदह", 14 )
  , ( "१४", 14 )
  , ( "पन्द्रह", 15 )
  , ( "१५", 15 )
  , ( "सोलह", 16 )
  , ( "१६", 16 )
  , ( "सत्रह", 17 )
  , ( "१७", 17 )
  , ( "अठारह", 18 )
  , ( "१८", 18 )
  , ( "उन्नीस", 19 )
  , ( "१९", 19 )
  ]

ruleElevenToNineteen :: Rule
ruleElevenToNineteen = Rule
  { name = "number (11..19)"
  , pattern =
    [ regex "(ग्यारह|११|बारह|१२|तेरह|१३|चौदह|१४|पन्द्रह|१५|सोलह|१६|सत्रह|१७|अठारह|१८|उन्नीस|१९)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) elevenToNineteenMap >>= integer
      _ -> Nothing
  }

twentyoneToTwentynineMap :: HashMap Text Integer
twentyoneToTwentynineMap = HashMap.fromList
  [ ( "इक्कीस", 21 )
  , ( "२१", 21 )
  , ( "बाईस", 22 )
  , ( "२२", 22 )
  , ( "तेईस", 23 )
  , ( "२३", 23 )
  , ( "चौबीस", 24 )
  , ( "२४", 24 )
  , ( "पच्चीस", 25 )
  , ( "२५", 25 )
  , ( "छब्बीस", 26 )
  , ( "२६", 26 )
  , ( "सत्ताईस", 27 )
  , ( "२७", 27 )
  , ( "अट्ठाईस", 28 )
  , ( "२८", 28 )
  , ( "उनतीस", 29 )
  , ( "२९", 29 )
  ]

ruleTwentyoneToTwentynine :: Rule
ruleTwentyoneToTwentynine = Rule
  { name = "number (21..29)"
  , pattern =
    [ regex "(इक्कीस|२१|बाईस|२२|तेईस|२३|चौबीस|२४|पच्चीस|२५|छब्बीस|२६|सत्ताईस|२७|अट्ठाईस|२८|उनतीस|२९)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) twentyoneToTwentynineMap >>= integer
      _ -> Nothing
  }

tensMap :: HashMap Text Integer
tensMap = HashMap.fromList
  [ ( "बीस", 20 )
  , ( "२०", 20 )
  , ( "तीस", 30 )
  , ( "३०", 30 )
  , ( "चालीस", 40 )
  , ( "४०", 40 )
  , ( "पचास", 50 )
  , ( "५०", 50 )
  , ( "साठ", 60 )
  , ( "६०", 60 )
  , ( "सत्तर", 70 )
  , ( "७०", 70 )
  , ( "अस्सी", 80 )
  , ( "८०", 80 )
  , ( "नब्बे", 90 )
  , ( "९०", 90 )
  ]

ruleTens :: Rule
ruleTens = Rule
  { name = "integer (20,30..90)"
  , pattern =
    [ regex "(बीस|२०|तीस|३०|चालीस|४०|पचास|५०|साठ|६०|सत्तर|७०|अस्सी|८०|नब्बे|९०)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) tensMap >>= integer
      _ -> Nothing
  }


rules :: [Rule]
rules =
  [ ruleIntegerNumeric
  , ruleNumeral
  , ruleElevenToNineteen
  , ruleTwentyoneToTwentynine
  , ruleTens
  ]
