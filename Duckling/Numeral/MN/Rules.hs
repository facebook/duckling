-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.MN.Rules
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

tensMap :: HashMap Text Integer
tensMap = HashMap.fromList
  [ ( "арван", 10)
  , ( "хорин", 20)
  , ( "гучин", 30)
  , ( "дөчин", 40)
  , ( "тавин", 50)
  , ( "жаран", 60)
  , ( "далан", 70)
  , ( "наян", 80)
  , ( "ерэн", 90)
  ]

ruleInteger5 :: Rule
ruleInteger5 = Rule
  { name = "integer (10..90)"
  , pattern =
    [ regex "(арван|хорин|гучин|дөчин|тавин|жаран|далан|наян|ерэн)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) tensMap >>= integer
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
    [ regex "(хоёр|хос)"
    ]
  , prod = \_ -> integer 2
  }

ruleDecimalOneAndAHalf :: Rule
ruleDecimalOneAndAHalf = Rule
  { name = "decimal one and a half"
   , pattern =
    [ regex "(хагас|тал)"
    ]
   , prod = \_ -> double 1.5
  }

ruleIntegerAndAHalf :: Rule
ruleIntegerAndAHalf = Rule
  { name = "<integer> and a half"
  , pattern =
    [ Predicate isNatural
    , regex " хагас"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v}:_) -> double $ v + 0.5
      _ -> Nothing
  }

hundredsMap :: HashMap Text Integer
hundredsMap = HashMap.fromList
  [ ( "зуу(н?)", 100)
  , ( "хоёр зуу(н?)", 200)
  , ( "гурван зуу(н?)", 300)
  , ( "дөрвөн зуу(н?)", 400)
  , ( "таван зуу(н?)", 500)
  , ( "зургаан зуу(н?)", 600)
  , ( "долоон зуу(н?)", 700)
  , ( "найман зуу(н?)", 800)
  , ( "есөн зуу(н?)", 900)
  ]

ruleInteger6 :: Rule
ruleInteger6 = Rule
  { name = "integer (100..900)"
  , pattern =
    [ regex "(зуу(н?)|хоёр зуу(н?)|гурван зуу(н?)|дөрвөн зуу(н?)|таван зуу(н?)|зургаан зуу(н?)|долоон зуу(н?)|найман зуу(н?)|есөн зуу(н?)"
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
    [ regex "-|хасах"
    , Predicate isPositive
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
    [ regex "(ноль|тэг|нойл)"
    ]
  , prod = \_ -> integer 0
  }

threeToNineteenMap:: HashMap Text Integer
threeToNineteenMap = HashMap.fromList
  [ ( "гурав", 3)
  , ( "дөрөв", 4)
  , ( "тав", 5)
  , ( "зургаа", 6)
  , ( "долоо", 7)
  , ( "найм", 8)
  , ( "ес", 9)
  , ( "арав", 10)
  , ( "арван нэг", 11)
  , ( "арван хоёр", 12)
  , ( "арван гурав", 13)
  , ( "арван дөрөв", 14)
  , ( "арван тав", 15)
  , ( "арван зургаа", 16)
  , ( "арван долоо", 17)
  , ( "арван найм", 18)
  , ( "арван ес", 19)
  ]

ruleInteger4 :: Rule
ruleInteger4 = Rule
  { name = "integer (3..19)"
  , pattern =
    [ regex "(гурав|дөрөв|тав|зургаа|долоо|найм|ес|арав|арван|арван нэг|арван хоёр|арван гурав|арван дөрөв|арван тав|арван зургаа|арван долоо|арван найм|арван ес)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) threeToNineteenMap >>= integer
      _ -> Nothing
  }

ruleInteger2 :: Rule
ruleInteger2 = Rule
  { name = "integer 1"
  , pattern =
    [ regex "(нэг|ганц)"
    ]
  , prod = \_ -> integer 1
  }

ruleNumeralDotNumeral :: Rule
ruleNumeralDotNumeral = Rule
  { name = "number dot number"
  , pattern =
    [ dimension Numeral
    , regex "цэг"
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
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        parseDouble (Text.replace "," Text.empty match) >>= double
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
  , ruleIntegerAndAHalf
  , ruleDecimalOneAndAHalf
  , ruleIntegerWithThousandsSeparator
  , ruleNumeralDotNumeral
  , ruleNumeralsPrefixWithMinus
  , ruleNumeralsSuffixesKMG
  ]
