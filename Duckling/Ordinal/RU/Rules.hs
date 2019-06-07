-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Ordinal.RU.Rules
  ( rules
  ) where

import Data.HashMap.Strict (HashMap)
import Data.String
import Prelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Ordinal.Helpers
import Duckling.Regex.Types
import Duckling.Types

ordinalsFirstthMap :: HashMap Text.Text Int
ordinalsFirstthMap = HashMap.fromList
  [ ( "перв", 1 )
  , ( "втор", 2 )
  , ( "трет", 3 )
  , ( "четверт", 4 )
  , ( "пят", 5 )
  , ( "шест", 6 )
  , ( "седьм", 7 )
  , ( "восьм", 8 )
  , ( "девят", 9 )
  , ( "десят", 10 )
  , ( "одинадцат", 11 )
  , ( "двенадцат", 12 )
  , ( "тринадцат", 13 )
  , ( "четырнадцат", 14 )
  , ( "пятнадцат", 15 )
  , ( "шестнадцат", 16 )
  , ( "семнадцат", 17 )
  , ( "восемнадцат", 18 )
  , ( "девятнадцат", 19 )
  , ( "двадцат", 20 )
  ]

cardinalsMap :: HashMap Text.Text Int
cardinalsMap = HashMap.fromList
  [ ( "двадцать", 20 )
  , ( "тридцать", 30 )
  , ( "сорок", 40 )
  , ( "пятьдесят", 50 )
  , ( "шестьдесят", 60 )
  , ( "семьдесят", 70 )
  , ( "восемьдесят", 80 )
  , ( "девяносто", 90 )
  ]

ruleOrdinalsFirstth :: Rule
ruleOrdinalsFirstth = Rule
  { name = "ordinals (first..19th)"
  , pattern =
    [ regex "(перв|втор|трет|четверт|пят|шест|седьм|восьм|девят|десят|одинадцат|двенадцат|тринадцат|четырнадцат|пятнадцат|шестнадцат|семнадцат|восемнадцат|девятнадцат|двадцат)(ье(го|й)?|ого|ый|ой|ий|ая|ое|ья)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        ordinal <$> HashMap.lookup (Text.toLower match) ordinalsFirstthMap
      _ -> Nothing
  }

ruleOrdinal :: Rule
ruleOrdinal = Rule
  { name = "ordinal 21..99"
  , pattern =
    [ regex "(двадцать|тридцать|сорок|пятьдесят|шестьдесят|семьдесят|восемьдесят|девяносто)"
    , regex "(перв|втор|трет|четверт|пят|шест|седьм|восьм|девят)(ье(го|й)?|ого|ый|ой|ий|ая|ое|ья)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:_)):
       Token RegexMatch (GroupMatch (m2:_)):
       _) -> do
         dozen <- HashMap.lookup (Text.toLower m1) cardinalsMap
         unit <- HashMap.lookup (Text.toLower m2) ordinalsFirstthMap
         Just . ordinal $ dozen + unit
      _ -> Nothing
  }

ruleOrdinalDigits :: Rule
ruleOrdinalDigits = Rule
  { name = "ordinal (digits)"
  , pattern =
    [ regex "0*(\\d+)-?((ы|о|и|а|e|ь)?(ее|й|я|е|го))"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> ordinal <$> parseInt match
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleOrdinal
  , ruleOrdinalDigits
  , ruleOrdinalsFirstth
  ]
