-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.HE.Rules
  ( rules
  ) where

import Data.Maybe
import Data.String
import Data.Text (Text)
import Prelude
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Regex.Types
import Duckling.Types
import qualified Duckling.Numeral.Types as TNumeral

ruleInteger5 :: Rule
ruleInteger5 = Rule
  { name = "integer 4"
  , pattern =
    [ regex "(ארבע(ה)?)"
    ]
  , prod = \_ -> integer 4
  }

ruleIntersectNumerals :: Rule
ruleIntersectNumerals = Rule
  { name = "intersect numbers"
  , pattern =
    [ Predicate hasGrain
    , Predicate $ and . sequence [not . isMultipliable, isPositive]
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = val1, TNumeral.grain = Just g}:
       Token Numeral NumeralData{TNumeral.value = val2}:
       _) | (10 ** fromIntegral g) > val2 -> double $ val1 + val2
      _ -> Nothing
  }

ruleIntersectWithAnd :: Rule
ruleIntersectWithAnd = Rule
  { name = "intersect (with and)"
  , pattern =
    [ Predicate hasGrain
    , regex "ו"
    , Predicate isMultipliable
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = val1, TNumeral.grain = Just g}:
       _:
       Token Numeral NumeralData{TNumeral.value = val2}:
       _) | (10 ** fromIntegral g) > val2 -> double $ val1 + val2
      _ -> Nothing
  }

ruleCompositeTens :: Rule
ruleCompositeTens = Rule
  { name = "integer 21..99"
  , pattern =
    [ oneOf [ 20, 30..90 ]
    , numberBetween 1 10
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = tens}:
       Token Numeral NumeralData{TNumeral.value = units}:
       _) -> double $ tens + units
      _ -> Nothing
  }

ruleCompositeTensWithAnd :: Rule
ruleCompositeTensWithAnd = Rule
  { name = "integer 21..99 (with and)"
  , pattern =
    [ oneOf [ 20, 30..90 ]
    , regex "ו"
    , numberBetween 1 10
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = tens}:
       _:
       Token Numeral NumeralData{TNumeral.value = units}:
       _) -> double $ tens + units
      _ -> Nothing
  }

ruleNumeralsPrefixWithNegativeOrMinus :: Rule
ruleNumeralsPrefixWithNegativeOrMinus = Rule
  { name = "numbers prefix with -, negative or minus"
  , pattern =
    [ regex "-|מינוס"
    , Predicate isPositive
    ]
  , prod = \tokens -> case tokens of
      (_:Token Numeral nd:_) -> double (TNumeral.value nd * (-1))
      _ -> Nothing
  }

ruleInteger10 :: Rule
ruleInteger10 = Rule
  { name = "integer 9"
  , pattern =
    [ regex "(תשע(ה)?)"
    ]
  , prod = \_ -> integer 9
  }

ruleInteger15 :: Rule
ruleInteger15 = Rule
  { name = "integer (20..90)"
  , pattern =
    [ regex "(עשרים|שלושים|ארבעים|חמישים|שישים|שבעים|שמונים|תשעים)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case match of
        "עשרים" -> integer 20
        "שלושים" -> integer 30
        "ארבעים" -> integer 40
        "חמישים" -> integer 50
        "שישים" -> integer 60
        "שבעים" -> integer 70
        "שמונים" -> integer 80
        "תשעים" -> integer 90
        _ -> Nothing
      _ -> Nothing
  }

ruleDecimalNumeral :: Rule
ruleDecimalNumeral = Rule
  { name = "decimal number"
  , pattern =
    [ regex "(\\d*\\.\\d+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> parseDecimal True match
      _ -> Nothing
  }

ruleInteger3 :: Rule
ruleInteger3 = Rule
  { name = "integer 2"
  , pattern =
    [ regex "(שתיים|שניים)"
    ]
  , prod = \_ -> integer 2
  }

ruleSingle :: Rule
ruleSingle = Rule
  { name = "single"
  , pattern =
    [ regex "יחיד"
    ]
  , prod = \_ -> integer 1
  }

ruleInteger13 :: Rule
ruleInteger13 = Rule
  { name = "integer 12"
  , pattern =
    [ regex "(שניים עשר|תרי עשר)"
    ]
  , prod = \_ -> integer 12
  }

ruleMultiply :: Rule
ruleMultiply = Rule
  { name = "compose by multiplication"
  , pattern =
    [ dimension Numeral
    , numberWith TNumeral.multipliable id
    ]
  , prod = \tokens -> case tokens of
      (token1:token2:_) -> multiply token1 token2
      _ -> Nothing
  }

ruleInteger6 :: Rule
ruleInteger6 = Rule
  { name = "integer 5"
  , pattern =
    [ regex "(חמ(ש|ישה))"
    ]
  , prod = \_ -> integer 5
  }

rulePowersOfTen :: Rule
rulePowersOfTen = Rule
  { name = "powers of tens"
  , pattern =
    [ regex "(מא(ה|ות)|אל(ף|פים)|מיליו(ן|נים))"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "מאה"                               ->
          double 1e2 >>= withGrain 2 >>= withMultipliable
        "מאות"                         ->
          double 1e2 >>= withGrain 2 >>= withMultipliable
        "אלף"                               ->
          double 1e3 >>= withGrain 3 >>= withMultipliable
        "אלפים"                   ->
          double 1e3 >>= withGrain 3 >>= withMultipliable
        "מיליון"             ->
          double 1e6 >>= withGrain 6 >>= withMultipliable
        "מיליונים" ->
          double 1e6 >>= withGrain 6 >>= withMultipliable
        _          -> Nothing
      _ -> Nothing
  }

ruleInteger7 :: Rule
ruleInteger7 = Rule
  { name = "integer 6"
  , pattern =
    [ regex "(שש(ה)?)"
    ]
  , prod = \_ -> integer 6
  }

ruleInteger14 :: Rule
ruleInteger14 = Rule
  { name = "integer 11..19"
  , pattern =
    [ numberBetween 1 10
    , numberWith TNumeral.value (== 10)
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v1}:
       Token Numeral NumeralData{TNumeral.value = v2}:
       _) -> double $ v1 + v2
      _ -> Nothing
  }

ruleInteger8 :: Rule
ruleInteger8 = Rule
  { name = "integer 7"
  , pattern =
    [ regex "(שבע(ה)?)"
    ]
  , prod = \_ -> integer 7
  }

ruleCouple :: Rule
ruleCouple = Rule
  { name = "couple"
  , pattern =
    [ regex "זוג( של)?"
    ]
  , prod = \_ -> integer 2
  }

ruleInteger16 :: Rule
ruleInteger16 = Rule
  { name = "integer 101..999"
  , pattern =
    [ oneOf [300, 600, 500, 100, 800, 200, 900, 700, 400]
    , numberBetween 1 100
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v1}:
       Token Numeral NumeralData{TNumeral.value = v2}:
       _) -> double $ v1 + v2
      _ -> Nothing
  }

ruleInteger9 :: Rule
ruleInteger9 = Rule
  { name = "integer 8"
  , pattern =
    [ regex "(שמונה)"
    ]
  , prod = \_ -> integer 8
  }

ruleInteger :: Rule
ruleInteger = Rule
  { name = "integer 0"
  , pattern =
    [ regex "(אפס|כלום)"
    ]
  , prod = \_ -> integer 0
  }

ruleInteger4 :: Rule
ruleInteger4 = Rule
  { name = "integer 3"
  , pattern =
    [ regex "(שלוש(ה)?)"
    ]
  , prod = \_ -> integer 3
  }

ruleInteger2 :: Rule
ruleInteger2 = Rule
  { name = "integer 1"
  , pattern =
    [ regex "(אחד|אחת)"
    ]
  , prod = \_ -> integer 1
  }

ruleInteger11 :: Rule
ruleInteger11 = Rule
  { name = "integer 10"
  , pattern =
    [ regex "(עשר(ה)?)"
    ]
  , prod = \_ -> integer 10
  }

ruleNumeralDotNumeral :: Rule
ruleNumeralDotNumeral = Rule
  { name = "number dot number"
  , pattern =
    [ dimension Numeral
    , regex "נקודה"
    , Predicate $ not . hasGrain
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral nd1:_:Token Numeral nd2:_) ->
        double $ TNumeral.value nd1 + decimalsToDouble (TNumeral.value nd2)
      _ -> Nothing
  }

ruleCommas :: Rule
ruleCommas = Rule
  { name = "comma-separated numbers"
  , pattern =
    [ regex "(\\d+(,\\d\\d\\d)+(\\.\\d+)?)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        parseDouble (Text.replace "," Text.empty match) >>= double
      _ -> Nothing
  }

ruleHalf :: Rule
ruleHalf = Rule
  { name = "half"
  , pattern =
    [ regex "חצי"
    ]
  , prod = \_ -> double 0.5
  }

rules :: [Rule]
rules =
  [ ruleCommas
  , ruleCompositeTens
  , ruleCompositeTensWithAnd
  , ruleCouple
  , ruleDecimalNumeral
  , ruleInteger
  , ruleInteger10
  , ruleInteger11
  , ruleInteger13
  , ruleInteger14
  , ruleInteger15
  , ruleInteger16
  , ruleInteger2
  , ruleInteger3
  , ruleInteger4
  , ruleInteger5
  , ruleInteger6
  , ruleInteger7
  , ruleInteger8
  , ruleInteger9
  , ruleIntersectNumerals
  , ruleIntersectWithAnd
  , ruleMultiply
  , ruleNumeralDotNumeral
  , ruleNumeralsPrefixWithNegativeOrMinus
  , rulePowersOfTen
  , ruleSingle
  , ruleHalf
  ]
