-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


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

devanagariMap :: HashMap Char Char
devanagariMap = HashMap.fromList
  [ ( '०', '0' )
  , ( '१', '1' )
  , ( '२', '2' )
  , ( '३', '3' )
  , ( '४', '4' )
  , ( '५', '5' )
  , ( '६', '6' )
  , ( '७', '7' )
  , ( '८', '8' )
  , ( '९', '9' )
  ]

devanagariToArab :: Char -> Char
devanagariToArab c = HashMap.lookupDefault c c devanagariMap

ruleDevanagari :: Rule
ruleDevanagari = Rule
  { name = "devanagari forms"
  , pattern =
    [ regex "([०१२३४५६७८९]{1,18})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        toInteger <$> parseInt (Text.map devanagariToArab match) >>= integer
      _ -> Nothing
  }

ruleNumeralMap :: HashMap Text Integer
ruleNumeralMap = HashMap.fromList
  [ ( "शून्य", 0 )
  , ( "एक", 1 )
  , ( "दो" , 2 )
  , ( "तीन", 3 )
  , ( "चार", 4 )
  , ( "पाँच", 5 )
  , ( "छह", 6 )
  , ( "सात", 7 )
  , ( "आठ", 8 )
  , ( "नौ" , 9 )
  , ( "दस", 10 )
  ]

ruleNumeral :: Rule
ruleNumeral = Rule
  { name = "number (0..10)"
  , pattern =
    [ regex "(शून्य|एक|दो|तीन|चार|पाँच|छह|सात|आठ|नौ|दस)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup match ruleNumeralMap >>= integer
      _ -> Nothing
  }

elevenToNineteenMap :: HashMap Text Integer
elevenToNineteenMap = HashMap.fromList
  [ ( "ग्यारह", 11 )
  , ( "बारह", 12 )
  , ( "तेरह", 13 )
  , ( "चौदह", 14 )
  , ( "पन्द्रह", 15 )
  , ( "सोलह", 16 )
  , ( "सत्रह", 17 )
  , ( "अठारह", 18 )
  , ( "उन्नीस", 19 )
  ]

ruleElevenToNineteen :: Rule
ruleElevenToNineteen = Rule
  { name = "number (11..19)"
  , pattern =
    [ regex "(ग्यारह|बारह|तेरह|चौदह|पन्द्रह|सोलह|सत्रह|अठारह|उन्नीस)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup match elevenToNineteenMap >>= integer
      _ -> Nothing
  }

twentyoneToTwentynineMap :: HashMap Text Integer
twentyoneToTwentynineMap = HashMap.fromList
  [ ( "इक्कीस", 21 )
  , ( "बाईस", 22 )
  , ( "तेईस", 23 )
  , ( "चौबीस", 24 )
  , ( "पच्चीस", 25 )
  , ( "छब्बीस", 26 )
  , ( "सत्ताईस", 27 )
  , ( "अट्ठाईस", 28 )
  , ( "उनतीस", 29 )
  ]

ruleTwentyoneToTwentynine :: Rule
ruleTwentyoneToTwentynine = Rule
  { name = "number (21..29)"
  , pattern =
    [ regex "(इक्कीस|बाईस|तेईस|चौबीस|पच्चीस|छब्बीस|सत्ताईस|अट्ठाईस|उनतीस)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup match twentyoneToTwentynineMap >>= integer
      _ -> Nothing
  }

tensMap :: HashMap Text Integer
tensMap = HashMap.fromList
  [ ( "बीस", 20 )
  , ( "तीस", 30 )
  , ( "चालीस", 40 )
  , ( "पचास", 50 )
  , ( "साठ", 60 )
  , ( "सत्तर", 70 )
  , ( "अस्सी", 80 )
  , ( "नब्बे", 90 )
  ]

ruleTens :: Rule
ruleTens = Rule
  { name = "integer (20,30..90)"
  , pattern =
    [ regex "(बीस|तीस|चालीस|पचास|साठ|सत्तर|अस्सी|नब्बे)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup match tensMap >>= integer
      _ -> Nothing
  }


rules :: [Rule]
rules =
  [ ruleDevanagari
  , ruleNumeral
  , ruleElevenToNineteen
  , ruleTwentyoneToTwentynine
  , ruleTens
  ]
