-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Ordinal.HI.Rules
  ( rules
  ) where

import Data.HashMap.Strict (HashMap)
import Data.String
import Data.Text (Text)
import Prelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Ordinal.Helpers
import Duckling.Regex.Types
import Duckling.Types

ordinalsMap :: HashMap Text Int
ordinalsMap = HashMap.fromList
  [ ( "शून्य" , 0 )
  , ( "प्रथम" , 1 )
  , ( "पहला" , 1 )
  , ( "पहली" , 1 )
  , ( "पहले" , 1 )
  , ( "द्वितीय" , 2 )
  , ( "दूसरा" , 2 )
  , ( "दूसरी" , 2 )
  , ( "दूसरे" , 2 )
  , ( "तृतीय" , 3 )
  , ( "तीसरा" , 3 )
  , ( "तीसरी" , 3 )
  , ( "तीसरे" , 3 )
  , ( "चौथा" , 4 )
  , ( "चौथी" , 4 )
  , ( "चौथे" , 4 )
  , ( "छठा" , 6 )
  , ( "छठी" , 6 )
  , ( "छठे" , 6 )
  ]

cardinalsMap :: HashMap Text Int
cardinalsMap = HashMap.fromList
  [ ( "पाँच", 5 )
  , ( "सात", 7 )
  , ( "आठ", 8 )
  , ( "नौ" , 9 )
  , ( "दस", 10 )
  , ( "ग्यारह", 11 )
  , ( "बारह", 12 )
  , ( "तेरह", 13 )
  , ( "चौदह", 14 )
  , ( "पन्द्रह", 15 )
  , ( "सोलह", 16 )
  , ( "सत्रह", 17 )
  , ( "अठारह", 18 )
  , ( "उन्नीस", 19 )
  , ( "बीस", 20 )
  , ( "इक्कीस", 21 )
  , ( "बाईस", 22 )
  , ( "तेईस", 23 )
  , ( "चौबीस", 24 )
  , ( "पच्चीस", 25 )
  , ( "छब्बीस", 26 )
  , ( "सत्ताईस", 27 )
  , ( "अट्ठाईस", 28 )
  , ( "उनतीस", 29 )
  , ( "तीस", 30 )
  , ( "चालीस", 40 )
  , ( "पचास", 50 )
  , ( "साठ", 60 )
  , ( "सत्तर", 70 )
  , ( "अस्सी", 80 )
  , ( "नब्बे", 90 )
  ]

ruleOrdinals :: Rule
ruleOrdinals = Rule
  { name = "ordinals (first..fourth, sixth)"
  , pattern = [regex "(शून्य|प्रथम|पहला|पहली|पहले|द्वितीय|दूसरा|दूसरी|दूसरे|तृतीय|तीसरा|तीसरी|तीसरे|चौथा|चौथी|चौथे|छठा|छठी|छठे)"]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        ordinal <$> HashMap.lookup (Text.toLower match) ordinalsMap
      _ -> Nothing
    }

ruleOtherOrdinals :: Rule
ruleOtherOrdinals = Rule
  { name = "ordinals (fifth, seventh ...)"
  , pattern = [regex "(पाँच|सात|आठ|नौ|दस|ग्यारह|बारह|तेरह|चौदह|पन्द्रह|सोलह|सत्रह|अठारह|उन्नीस|बीस|इक्कीस|बाईस|तेईस|चौबीस|पच्चीस|छब्बीस|सत्ताईस|अट्ठाईस|उनतीस|तीस|चालीस|पचास|साठ|सत्तर|अस्सी|नब्बे)(वा|वी|वे)"]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        ordinal <$> HashMap.lookup (Text.toLower match) cardinalsMap
      _ -> Nothing
  }


ruleOrdinalDigits :: Rule
ruleOrdinalDigits = Rule
  { name = "ordinal (digits)"
  , pattern = [regex "0*(\\d+) ?(वा|वी|वे)"]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> ordinal <$> parseInt match
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleOrdinals
  , ruleOtherOrdinals
  , ruleOrdinalDigits
  ]
