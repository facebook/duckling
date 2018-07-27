-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoRebindableSyntax #-}

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
  [ ( "សូន្យ", 0 )
  , ( "មួយ", 1 )
  , ( "ពីរ", 2 )
  , ( "បី", 3 )
  , ( "បួន", 4 )
  , ( "ប្រាំ", 5)
  , ( "ប្រាំមួយ", 6)
  , ( "ប្រាំពីរ", 7)
  , ( "ប្រាំបី", 8)
  , ( "ប្រាំបួន", 9)
  , ( "ដប់", 10)
  ]

ruleNumeral :: Rule
ruleNumeral = Rule
  { name = "number (0..10)"
  , pattern =
    [ regex "(សូន្យ|មួយ|ពីរ|បី|បួន|ប្រាំ|ប្រាំមួយ|ប្រាំពីរ|ប្រាំបី|ប្រាំបួន|ដប់)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) ruleNumeralMap >>= integer
      _ -> Nothing
  }

elevenToNineteenMap :: HashMap Text Integer
elevenToNineteenMap = HashMap.fromList
  [ ( "ដប់មួយ", 11 )
  , ( "ដប់ពីរ", 12 )
  , ( "ដប់បី", 13 )
  , ( "ដប់បួន", 14 )
  , ( "ដប់ប្រាំ", 15 )
  , ( "ដប់ប្រាំមួយ", 16 )
  , ( "ដប់ប្រាំពីរ", 17 )
  , ( "ដប់ប្រាំបី", 18 )
  , ( "ដប់ប្រាំបួន", 19 )
  ]

ruleElevenToNineteen :: Rule
ruleElevenToNineteen = Rule
  { name = "number (11..19)"
  , pattern =
    [ regex "(ដប់មួយ|ដប់ពីរ|ដប់បី|ដប់បួន|ដប់ប្រាំ|ដប់ប្រាំមួយ|ដប់ប្រាំពីរ|ដប់ប្រាំបី|ដប់ប្រាំបួន)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) elevenToNineteenMap >>= integer
      _ -> Nothing
  }

twentyoneToTwentynineMap :: HashMap Text Integer
twentyoneToTwentynineMap = HashMap.fromList
  [ ( "ម្ភៃមួយ", 21 )
  , ( "ម្ភៃពីរ", 22 )
  , ( "ម្ភៃបី", 23 )
  , ( "ម្ភៃបួន", 24 )
  , ( "ម្ភៃប្រាំ", 25 )
  , ( "ម្ភៃប្រាំមួយ", 26 )
  , ( "ម្ភៃប្រាំពីរ", 27 )
  , ( "ម្ភៃប្រាំបី", 28 )
  , ( "ម្ភៃប្រាំបួន", 29 )
  ]

ruleTwentyoneToTwentynine :: Rule
ruleTwentyoneToTwentynine = Rule
  { name = "number (21..29)"
  , pattern =
    [ regex "(ម្ភៃមួយ|ម្ភៃពីរ|ម្ភៃបី|ម្ភៃបួន|ម្ភៃប្រាំ|ម្ភៃប្រាំមួយ|ម្ភៃប្រាំពីរ|ម្ភៃប្រាំបី|ម្ភៃប្រាំបួន)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) twentyoneToTwentynineMap >>= integer
      _ -> Nothing
  }

dozensMap :: HashMap Text Integer
dozensMap = HashMap.fromList
  [ ( "ម្ភៃ", 20 )
  , ( "សាមសិប", 30 )
  , ( "សែសិប", 40 )
  , ( "ហាសិប", 50 )
  , ( "ហុកសិប", 60 )
  , ( "ចិតសិប", 70 )
  , ( "ប៉ែតសិប", 80 )
  , ( "កៅសិប", 90 )
  ]

ruleTens :: Rule
ruleTens = Rule
  { name = "integer (20,30..90)"
  , pattern =
    [ regex "(ម្ភៃ|សាមសិប|សែសិប|ហាសិប|ហុកសិប|ចិតសិប|ប៉ែតសិប|កៅសិប)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) dozensMap >>= integer
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
  
