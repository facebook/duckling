-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.JA.Rules
  ( rules
  ) where

import Data.HashMap.Strict (HashMap)
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

ruleInteger5 :: Rule
ruleInteger5 = Rule
  { name = "integer (100)"
  , pattern =
    [ regex "百"
    ]
  , prod = \_ -> integer 100
  }

ruleNumeralsPrefixWithNegativeOrMinus :: Rule
ruleNumeralsPrefixWithNegativeOrMinus = Rule
  { name = "numbers prefix with -, negative or minus"
  , pattern =
    [ regex "-|マイナス\\s?|負\\s?"
    , Predicate isPositive
    ]
  , prod = \tokens -> case tokens of
      (_:Token Numeral nd:_) -> double (TNumeral.value nd * (-1))
      _ -> Nothing
  }

ruleInteger10 :: Rule
ruleInteger10 = Rule
  { name = "integer (1000..1999)"
  , pattern =
    [ regex "千"
    , numberBetween 1 1000
    ]
  , prod = \tokens -> case tokens of
      (_:Token Numeral NumeralData{TNumeral.value = v}:_) ->
        double $ v + 1000
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

ruleInteger15 :: Rule
ruleInteger15 = Rule
  { name = "integer (20000..90000)"
  , pattern =
    [ numberBetween 2 10
    , regex "万"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v}:_) -> double $ v * 10000
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

ruleNumeral :: Rule
ruleNumeral = Rule
  { name = "<number>个"
  , pattern =
    [ dimension Numeral
    , regex "个"
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> Just token
      _ -> Nothing
  }

ruleInteger3 :: Rule
ruleInteger3 = Rule
  { name = "integer (20..90)"
  , pattern =
    [ numberBetween 2 10
    , regex "十"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v}:_) -> double $ v * 10
      _ -> Nothing
  }

ruleInteger13 :: Rule
ruleInteger13 = Rule
  { name = "integer (10000)"
  , pattern =
    [ regex "万"
    ]
  , prod = \_ -> integer 10000
  }

ruleInteger6 :: Rule
ruleInteger6 = Rule
  { name = "integer (100..199)"
  , pattern =
    [ regex "百"
    , numberBetween 1 100
    ]
  , prod = \tokens -> case tokens of
      (_:Token Numeral NumeralData{TNumeral.value = v}:_) -> double $ v + 100
      _ -> Nothing
  }

ruleInteger12 :: Rule
ruleInteger12 = Rule
  { name = "integer 2001..9999"
  , pattern =
    [ oneOf [3000, 9000, 7000, 8000, 2000, 4000, 6000, 5000]
    , numberBetween 1 1000
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v1}:
       Token Numeral NumeralData{TNumeral.value = v2}:
       _) -> double $ v1 + v2
      _ -> Nothing
  }

ruleNumeralsSuffixesKMG :: Rule
ruleNumeralsSuffixesKMG = Rule
  { name = "numbers suffixes (K, M, G, 千, 万)"
  , pattern =
    [ dimension Numeral
    , regex "(k|m|g|千|万)"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v}:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> case Text.toLower match of
          "k"      -> double $ v * 1e3
          "千" -> double $ v * 1e3
          "万" -> double $ v * 1e4
          "m"      -> double $ v * 1e6
          "g"      -> double $ v * 1e9
          _ -> Nothing
      _ -> Nothing
  }

ruleInteger7 :: Rule
ruleInteger7 = Rule
  { name = "integer (200..900)"
  , pattern =
    [ numberBetween 2 10
    , regex "百"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v}:_) -> double $ v * 100
      _ -> Nothing
  }

ruleInteger14 :: Rule
ruleInteger14 = Rule
  { name = "integer (10000..19999)"
  , pattern =
    [ regex "万"
    , numberBetween 1 10000
    ]
  , prod = \tokens -> case tokens of
      (_:Token Numeral NumeralData{TNumeral.value = v}:_) ->
        double $ v + 10000
      _ -> Nothing
  }

ruleInteger8 :: Rule
ruleInteger8 = Rule
  { name = "integer 201..999"
  , pattern =
    [ oneOf [300, 600, 500, 800, 200, 900, 700, 400]
    , numberBetween 1 100
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v1}:
       Token Numeral NumeralData{TNumeral.value = v2}:
       _) -> double $ v1 + v2
      _ -> Nothing
  }

ruleInteger16 :: Rule
ruleInteger16 = Rule
  { name = "integer 20001..99999"
  , pattern =
    [ oneOf [20000, 40000, 80000, 60000, 30000, 70000, 90000, 50000]
    , numberBetween 1 10000
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v1}:
       Token Numeral NumeralData{TNumeral.value = v2}:
       _) -> double $ v1 + v2
      _ -> Nothing
  }

ruleInteger9 :: Rule
ruleInteger9 = Rule
  { name = "integer (1000)"
  , pattern =
    [ regex "千"
    ]
  , prod = \_ -> integer 1000
  }

integerMap :: HashMap Text Integer
integerMap = HashMap.fromList
  [ ("零", 0)
  , ("ゼロ", 0)
  , ("一", 1)
  , ("二", 2)
  , ("三", 3)
  , ("四", 4)
  , ("五", 5)
  , ("六", 6)
  , ("七", 7)
  , ("八", 8)
  , ("九", 9)
  , ("十", 10)
  ]

ruleInteger :: Rule
ruleInteger = Rule
  { name = "integer (0..10)"
  , pattern =
    [ regex "(ゼロ|零|一|二|三|四|五|六|七|八|九|十)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) integerMap >>= integer
      _ -> Nothing
  }

ruleInteger4 :: Rule
ruleInteger4 = Rule
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

ruleInteger2 :: Rule
ruleInteger2 = Rule
  { name = "integer (11..19)"
  , pattern =
    [ regex "十"
    , numberBetween 1 10
    ]
  , prod = \tokens -> case tokens of
      (_:Token Numeral NumeralData{TNumeral.value = v}:_) -> double $ v + 10
      _ -> Nothing
  }

ruleInteger11 :: Rule
ruleInteger11 = Rule
  { name = "integer (2000..9000)"
  , pattern =
    [ numberBetween 2 10
    , regex "千"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v}:_) -> double $ v * 1000
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
  , ruleInteger10
  , ruleInteger11
  , ruleInteger12
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
  , ruleIntegerWithThousandsSeparator
  , ruleNumeral
  , ruleNumeralsPrefixWithNegativeOrMinus
  , ruleNumeralsSuffixesKMG
  ]
