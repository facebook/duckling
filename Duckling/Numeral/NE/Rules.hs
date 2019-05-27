-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Numeral.NE.Rules
  ( rules
  ) where

import Control.Applicative ((<|>))
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

zeroToNineteenMap :: HashMap Text Integer
zeroToNineteenMap = HashMap.fromList
  [ ( "शुन्य", 0 )
  , ( "सुन्ना", 0 )
  , ( "एक", 1 )
  , ( "दुई", 2 )
  , ( "तीन", 3 )
  , ( "चार", 4 )
  , ( "पाँच", 5 )
  , ( "छ", 6 )
  , ( "सात", 7 )
  , ( "आठ", 8 )
  , ( "नौ", 9 )
  , ( "दश", 10 )
  , ( "एघार", 11 )
  , ( "बाह्र", 12 )
  , ( "तेह्र", 13 )
  , ( "चौध", 14 )
  , ( "पन्ध्र", 15 )
  , ( "सोह्र", 16 )
  , ( "सत्र", 17 )
  , ( "अठार", 18 )
  , ( "उन्नाइस", 19 )
  ]

ruleToNineteen :: Rule
ruleToNineteen = Rule
  { name = "integer (0..19)"
  , pattern =
    [ regex "(शुन्य|सुन्ना|एक|दुई|तीन|चार|पाँच|छ|सात|आठ|नौ|दश|एघार|बाह्र|तेह्र|चौध|पन्ध्र|सोह्र|सत्र|अठार|उन्नाइस)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) zeroToNineteenMap >>= integer
      _ -> Nothing
  }

twentyoneToTwentynineMap :: HashMap Text Integer
twentyoneToTwentynineMap = HashMap.fromList
  [ ( "एक्काइस", 21 )
  , ( "बाइस", 22 )
  , ( "तेइस", 23 )
  , ( "चौबिस", 24 )
  , ( "पच्चिस", 25 )
  , ( "छब्बिस", 26 )
  , ( "सत्ताइस", 27 )
  , ( "अट्ठाइस", 28 )
  , ( "उनन्तिस", 29 )
  ]

ruleTwentyoneToTwentynine :: Rule
ruleTwentyoneToTwentynine = Rule
  { name = "number (21..29)"
  , pattern =
    [ regex "(एक्काइस|बाइस|तेइस|चौबिस|पच्चिस|छब्बिस|सत्ताइस|अट्ठाइस|उनन्तिस)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) twentyoneToTwentynineMap >>= integer
      _ -> Nothing
  }

tensMap :: HashMap Text Integer
tensMap = HashMap.fromList
  [ ( "बिस", 20 )
  , ( "तिस", 30 )
  , ( "चालिस", 40 )
  , ( "पचास", 50 )
  , ( "साठी", 60 )
  , ( "सत्तरी", 70 )
  , ( "असी", 80 )
  , ( "नब्बे", 90 )
  ]

ruleTens :: Rule
ruleTens = Rule
  { name = "integer (20..90)"
  , pattern =
    [ regex "(बिस|तिस|चालिस|पचास|साठी|सत्तरी|असी|नब्बे)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) tensMap >>= integer
      _ -> Nothing
  }


rules :: [Rule]
rules =
  [ ruleToNineteen
  , ruleTwentyoneToTwentynine
  , ruleTens
  ]
