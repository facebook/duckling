-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Ordinal.EN.Rules
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
  [ ( "first", 1 )
  , ( "second", 2 )
  , ( "third", 3 )
  , ( "fourth", 4 )
  , ( "fifth", 5 )
  , ( "sixth", 6 )
  , ( "seventh", 7 )
  , ( "eighth", 8 )
  , ( "ninth", 9 )
  , ( "tenth", 10 )
  , ( "eleventh", 11 )
  , ( "twelfth", 12 )
  , ( "thirteenth", 13 )
  , ( "fourteenth", 14 )
  , ( "fifteenth", 15 )
  , ( "sixteenth", 16 )
  , ( "seventeenth", 17 )
  , ( "eighteenth", 18 )
  , ( "nineteenth", 19 )
  , ( "twentieth", 20 )
  , ( "thirtieth", 30 )
  , ( "fortieth", 40 )
  , ( "fiftieth", 50 )
  , ( "sixtieth", 60 )
  , ( "seventieth", 70 )
  , ( "eightieth", 80 )
  , ( "ninetieth", 90 )
  ]

cardinalsMap :: HashMap Text Int
cardinalsMap = HashMap.fromList
  [ ( "twenty", 20 )
  , ( "thirty", 30 )
  , ( "forty", 40 )
  , ( "fifty", 50 )
  , ( "sixty", 60 )
  , ( "seventy", 70 )
  , ( "eighty", 80 )
  , ( "ninety", 90 )
  ]

ruleOrdinals :: Rule
ruleOrdinals = Rule
  { name = "ordinals (first..twentieth,thirtieth,...)"
  , pattern = [regex "(first|second|third|fourth|fifth|sixth|seventh|eighth|ninth|tenth|eleventh|twelfth|thirteenth|fourteenth|fifteenth|sixteenth|seventeenth|eighteenth|nineteenth|twentieth|thirtieth|fortieth|fiftieth|sixtieth|seventieth|eightieth|ninetieth)"]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        ordinal <$> HashMap.lookup (Text.toLower match) ordinalsMap
      _ -> Nothing
    }

ruleCompositeOrdinals :: Rule
ruleCompositeOrdinals = Rule
  { name = "ordinals (composite, e.g. eighty-seven, forty—seventh, twenty ninth, thirtythird)"
  , pattern = [regex "(twenty|thirty|forty|fifty|sixty|seventy|eighty|ninety)[\\s\\-\\—]?(first|second|third|fourth|fifth|sixth|seventh|eighth|ninth)"]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (tens:units:_)):_) -> do
        tt <- HashMap.lookup (Text.toLower tens) cardinalsMap
        uu <- HashMap.lookup (Text.toLower units) ordinalsMap
        Just (ordinal (tt + uu))
      _ -> Nothing
  }

ruleOrdinalDigits :: Rule
ruleOrdinalDigits = Rule
  { name = "ordinal (digits)"
  , pattern = [regex "0*(\\d+) ?(st|nd|rd|th)"]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> ordinal <$> parseInt match
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleOrdinals
  , ruleCompositeOrdinals
  , ruleOrdinalDigits
  ]
