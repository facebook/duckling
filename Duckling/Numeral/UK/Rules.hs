-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.UK.Rules
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

twentyNinetyMap :: HashMap Text Integer
twentyNinetyMap = HashMap.fromList
  [ ( "двадцять"  , 20 )
  , ( "тридцять"  , 30 )
  , ( "сорок"     , 40 )
  , ( "п‘ятдесят" , 50 )
  , ( "шістдесят" , 60 )
  , ( "сімдесят"  , 70 )
  , ( "дев‘яносто", 90 )
  , ( "вісімдесят", 80 )
  ]

ruleInteger5 :: Rule
ruleInteger5 = Rule
  { name = "integer (20..90)"
  , pattern =
    [ regex "(двадцять|тридцять|сорок|п‘ятдесят|шістдесят|сімдесят|вісімдесят|дев‘яносто)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) twentyNinetyMap >>= integer
      _ -> Nothing
  }

ruleDecimalWithThousandsSeparator :: Rule
ruleDecimalWithThousandsSeparator = Rule
  { name = "decimal with thousands separator"
  , pattern =
    [ regex "(\\d+(,\\d\\d\\d)+\\.\\d+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        parseDouble (Text.replace "," Text.empty match) >>= double
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
    [ regex "(два|дві|двоє|пара|пару|парочку|парочка)"
    ]
  , prod = \_ -> integer 2
  }

hundredsMap :: HashMap Text Integer
hundredsMap = HashMap.fromList
  [ ( "сто"                                     , 100 )
  , ( "двісті"                   , 200 )
  , ( "триста"                   , 300 )
  , ( "чотириста" , 400 )
  , ( "п‘ятсот"             , 500 )
  , ( "шістсот"             , 600 )
  , ( "сімсот"                   , 700 )
  , ( "вісімсот"       , 800 )
  , ( "дев‘ятсот" , 900 )
  ]
ruleInteger6 :: Rule
ruleInteger6 = Rule
  { name = "integer (100..900)"
  , pattern =
    [ regex "(сто|двісті|триста|чотириста|п‘ятсот|шістсот|сімсот|вісімсот|дев‘ятсот)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) hundredsMap >>= integer
      _ -> Nothing
  }

ruleNumeralsPrefixWithMinus :: Rule
ruleNumeralsPrefixWithMinus = Rule
  { name = "numbers prefix with -, minus"
  , pattern =
    [ regex "-|мінус\\s?"
    , dimension Numeral
    ]
  , prod = \tokens -> case tokens of
      (_:Token Numeral nd:_) -> double (TNumeral.value nd * (-1))
      _ -> Nothing
  }

ruleNumeralsSuffixesKMG :: Rule
ruleNumeralsSuffixesKMG = Rule
  { name = "numbers suffixes (K, M, G)"
  , pattern =
    [ dimension Numeral
    , regex "((к|м|г)|(К|М|Г))(?=[\\W\\$€]|$)"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v}:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> case Text.toLower match of
         "к" -> double $ v * 1e3
         "К" -> double $ v * 1e3
         "м" -> double $ v * 1e6
         "М" -> double $ v * 1e6
         "г" -> double $ v * 1e9
         "Г" -> double $ v * 1e9
         _   -> Nothing
      _ -> Nothing
  }

ruleInteger7 :: Rule
ruleInteger7 = Rule
  { name = "integer 21..99"
  , pattern =
    [ oneOf [70, 20, 60, 50, 40, 90, 30, 80]
    , numberBetween 1 10
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v1}:
       Token Numeral NumeralData{TNumeral.value = v2}:
       _) -> double $ v1 + v2
      _ -> Nothing
  }

ruleInteger8 :: Rule
ruleInteger8 = Rule
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

ruleInteger :: Rule
ruleInteger = Rule
  { name = "integer 0"
  , pattern =
    [ regex "(нуль)"
    ]
  , prod = \_ -> integer 0
  }

threeNineteenMap :: HashMap Text Integer
threeNineteenMap = HashMap.fromList
  [ ( "три"                                           , 3 )
  , ( "чотири"                         , 4 )
  , ( "п‘ять"                               , 5 )
  , ( "шість"                               , 6 )
  , ( "сім"                                           , 7 )
  , ( "вісім"                               , 8 )
  , ( "дев‘ять"                   , 9 )
  , ( "десять"                         , 10 )
  , ( "одинадцять" , 11 )
  , ( "дванадцять" , 12 )
  , ( "тринадцять" , 13 )
  , ( "чотирнадцять" , 14 )
  , ( "п‘ятнадцять"       , 15 )
  , ( "шістнадцять"       , 16 )
  , ( "сімнадцять"             , 17 )
  , ( "вісімнадцять" , 18 )
  , ( "дев‘ятнадцять" , 19 )
  ]
ruleInteger4 :: Rule
ruleInteger4 = Rule
  { name = "integer (3..19)"
  , pattern =
    [ regex "(три|чотирнадцять|чотири|п‘ятнадцять|п‘ять|шістнадцять|шість|сімнадцять|сім|вісімнадцять|вісім|дев‘ятнадцять|дев‘ять|десять|одинадцять|дванадцять|тринадцять)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) threeNineteenMap >>= integer
      _ -> Nothing
  }

ruleInteger2 :: Rule
ruleInteger2 = Rule
  { name = "integer 1"
  , pattern =
    [ regex "(один|одна|одну|одне|одного)"
    ]
  , prod = \_ -> integer 1
  }

ruleNumeralDotNumeral :: Rule
ruleNumeralDotNumeral = Rule
  { name = "number dot number"
  , pattern =
    [ dimension Numeral
    , regex "крапка"
    , Predicate $ not . hasGrain
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral nd1:_:Token Numeral nd2:_) ->
        double $ TNumeral.value nd1 + decimalsToDouble (TNumeral.value nd2)
      _ -> Nothing
  }

ruleIntegerWithThousandsSeparator :: Rule
ruleIntegerWithThousandsSeparator = Rule
  { name = "integer with thousands separator ,"
  , pattern =
    [ regex "(\\d{1,3}(,\\d\\d\\d){1,5})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):
       _) -> let fmt = Text.replace "," Text.empty match
        in parseDouble fmt >>= double
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleDecimalNumeral
  , ruleDecimalWithThousandsSeparator
  , ruleInteger
  , ruleInteger2
  , ruleInteger3
  , ruleInteger4
  , ruleInteger5
  , ruleInteger6
  , ruleInteger7
  , ruleInteger8
  , ruleIntegerWithThousandsSeparator
  , ruleNumeralDotNumeral
  , ruleNumeralsPrefixWithMinus
  , ruleNumeralsSuffixesKMG
  ]
