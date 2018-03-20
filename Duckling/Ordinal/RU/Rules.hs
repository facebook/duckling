-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Ordinal.RU.Rules
  ( rules
  ) where

import Data.String
import Prelude
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Ordinal.Helpers
import Duckling.Regex.Types
import Duckling.Types

ruleOrdinalsFirstth :: Rule
ruleOrdinalsFirstth = Rule
  { name = "ordinals (first..19th)"
  , pattern =
    [ regex "(перв|втор|трет|четверт|пят|шест|седьм|восьм|девят|десят|одинадцат|двенадцат|тринадцат|четырнадцат|пятнадцат|шестнадцат|семнадцат|восемнадцат|девятнадцат|двадцат)(ье(го|й)?|ого|ый|ой|ий|ая|ое|ья)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "перв" -> Just $ ordinal 1
        "втор" -> Just $ ordinal 2
        "трет" -> Just $ ordinal 3
        "четверт" -> Just $ ordinal 4
        "пят" -> Just $ ordinal 5
        "шест" -> Just $ ordinal 6
        "седьм" -> Just $ ordinal 7
        "восьм" -> Just $ ordinal 8
        "девят" -> Just $ ordinal 9
        "десят" -> Just $ ordinal 10
        "одинадцат" -> Just $ ordinal 11
        "двенадцат" -> Just $ ordinal 12
        "тринадцат" -> Just $ ordinal 13
        "четырнадцат" -> Just $ ordinal 14
        "пятнадцат" -> Just $ ordinal 15
        "шестнадцат" -> Just $ ordinal 16
        "семнадцат" -> Just $ ordinal 17
        "восемнадцат" -> Just $ ordinal 18
        "девятнадцат" -> Just $ ordinal 19
        "двадцат" -> Just $ ordinal 20
        _ -> Nothing
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
         dozen <- case Text.toLower m1 of
           "двадцать" -> Just 20
           "тридцать" -> Just 30
           "сорок" -> Just 40
           "пятьдесят" -> Just 50
           "шестьдесят" -> Just 60
           "семьдесят" -> Just 70
           "восемьдесят" -> Just 80
           "девяносто" -> Just 90
           _ -> Nothing
         unit <- case Text.toLower m2 of
           "перв" -> Just 1
           "втор" -> Just 2
           "трет" -> Just 3
           "четверт" -> Just 4
           "пят" -> Just 5
           "шест" -> Just 6
           "седьм" -> Just 7
           "восьм" -> Just 8
           "девят" -> Just 9
           _ -> Nothing
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
