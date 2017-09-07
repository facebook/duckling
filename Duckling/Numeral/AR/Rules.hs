-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.AR.Rules
  ( rules ) where

import Data.Maybe
import qualified Data.Text as Text
import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers
import Duckling.Numeral.Types (NumeralData (..))
import qualified Duckling.Numeral.Types as TNumeral
import Duckling.Regex.Types
import Duckling.Types

ruleInteger5 :: Rule
ruleInteger5 = Rule
  { name = "integer 4"
  , pattern =
    [ regex "(أربع(ة)?)"
    ]
  , prod = \_ -> integer 4
  }

ruleInteger23 :: Rule
ruleInteger23 = Rule
  { name = "integer 101..999"
  , pattern =
    [ oneOf [100, 200 .. 900]
    , regex "و"
    , numberBetween 1 100
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = v1}):
       _:
       Token Numeral (NumeralData {TNumeral.value = v2}):
       _) -> double $ v1 + v2
      _ -> Nothing
  }

ruleInteger18 :: Rule
ruleInteger18 = Rule
  { name = "integer 12"
  , pattern =
    [ regex "(إثن(ت)?ى عشر)"
    ]
  , prod = \_ -> integer 12
  }

ruleIntegerNumeric :: Rule
ruleIntegerNumeric = Rule
  { name = "integer (numeric)"
  , pattern =
    [ regex "(\\d{1,18})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        v <- parseInt match
        integer $ toInteger v
      _ -> Nothing
  }

ruleInteger19 :: Rule
ruleInteger19 = Rule
  { name = "integer (20..90)"
  , pattern =
    [ regex "(عشرون|ثلاثون|أربعون|خمسون|ستون|سبعون|ثمانون|تسعون)"
    ]
  , prod = \tokens -> case tokens of
      Token RegexMatch (GroupMatch (match:_)):_ -> case match of
        "عشرون" -> integer 20
        "ثلاثون" -> integer 30
        "أربعون" -> integer 40
        "خمسون" -> integer 50
        "ستون" -> integer 60
        "سبعون" -> integer 70
        "ثمانون" -> integer 80
        "تسعون" -> integer 90
        _ -> Nothing
      _ -> Nothing
  }

ruleInteger22 :: Rule
ruleInteger22 = Rule
  { name = "integer 21..99"
  , pattern =
    [ numberBetween 1 10
    , regex "و"
    , oneOf [20, 30 .. 90]
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = v1}):
       _:
       Token Numeral (NumeralData {TNumeral.value = v2}):
       _) -> double $ v1 + v2
      _ -> Nothing
  }

ruleInteger21 :: Rule
ruleInteger21 = Rule
  { name = "integer (13..19)"
  , pattern =
    [ numberBetween 3 10
    , numberWith TNumeral.value (== 10)
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = v}):_) -> double $ v + 10
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
        parseDouble (Text.replace (Text.singleton ',') Text.empty match) >>= double
      _ -> Nothing
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

ruleInteger15 :: Rule
ruleInteger15 = Rule
  { name = "integer 11"
  , pattern =
    [ regex "(إحدى عشر(ة)?)"
    ]
  , prod = \_ -> integer 11
  }

ruleDecimalNumeral :: Rule
ruleDecimalNumeral = Rule
  { name = "decimal number"
  , pattern =
    [ regex "(\\d*\\.\\d+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        parseDecimal True match
      _ -> Nothing
  }

rulePowersOfTen :: Rule
rulePowersOfTen = Rule
  { name = "powers of tens"
  , pattern =
    [ regex "(مائة|مئات|ألف|الف|آلاف|ملايي(ن)?)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "مائة" ->
          double 1e2 >>= withGrain 2 >>= withMultipliable
        "مئات" ->
          double 1e2 >>= withGrain 2 >>= withMultipliable
        "ألف" -> double 1e3 >>= withGrain 3 >>= withMultipliable
        "الف" -> double 1e3 >>= withGrain 3 >>= withMultipliable
        "آلاف" ->
          double 1e3 >>= withGrain 3 >>= withMultipliable
        "ملايي" ->
          double 1e6 >>= withGrain 6 >>= withMultipliable
        "ملايين" ->
          double 1e6 >>= withGrain 6 >>= withMultipliable
        _ -> Nothing
      _ -> Nothing
  }

ruleInteger3 :: Rule
ruleInteger3 = Rule
  { name = "integer 2"
  , pattern =
    [ regex "(اثنان|اثنين)"
    ]
  , prod = \_ -> integer 2
  }

ruleInteger13 :: Rule
ruleInteger13 = Rule
  { name = "integer 9"
  , pattern =
    [ regex "(تسعة|تسع)"
    ]
  , prod = \_ -> integer 9
  }

ruleInteger12 :: Rule
ruleInteger12 = Rule
  { name = "integer 8"
  , pattern =
    [ regex "(ثمانية|ثمان)"
    ]
  , prod = \_ -> integer 8
  }

ruleNumeralsPrefixWithMinus :: Rule
ruleNumeralsPrefixWithMinus = Rule
  { name = "numbers prefix with -, minus"
  , pattern =
    [ regex "-"
    , dimension Numeral
    ]
  , prod = \tokens -> case tokens of
      (_:Token Numeral (NumeralData {TNumeral.value = v}):_) ->
        double (v * (- 1))
      _ -> Nothing
  }

ruleInteger7 :: Rule
ruleInteger7 = Rule
  { name = "integer 5"
  , pattern =
    [ regex "(خمس)(ة)?"
    ]
  , prod = \_ -> integer 5
  }

ruleInteger14 :: Rule
ruleInteger14 = Rule
  { name = "integer 10"
  , pattern =
    [ regex "(عشرة|عشر)"
    ]
  , prod = \_ -> integer 10
  }

ruleInteger9 :: Rule
ruleInteger9 = Rule
  { name = "integer 6"
  , pattern =
    [ regex "(ست(ة)?)"
    ]
  , prod = \_ -> integer 6
  }

ruleInteger :: Rule
ruleInteger = Rule
  { name = "integer 0"
  , pattern =
    [ regex "(صفر)"
    ]
  , prod = \_ -> integer 0
  }

ruleInteger4 :: Rule
ruleInteger4 = Rule
  { name = "integer 3"
  , pattern =
    [ regex "(ثلاث|ثلاثة)"
    ]
  , prod = \_ -> integer 3
  }

ruleInteger2 :: Rule
ruleInteger2 = Rule
  { name = "integer 1"
  , pattern =
    [ regex "(واحدة|واحده|واحد)"
    ]
  , prod = \_ -> integer 1
  }

ruleInteger11 :: Rule
ruleInteger11 = Rule
  { name = "integer 7"
  , pattern =
    [ regex "(سبعة|سبع)"
    ]
  , prod = \_ -> integer 7
  }

ruleInteger20 :: Rule
ruleInteger20 = Rule
  { name = "integer (100..900)"
  , pattern =
    [ regex "(مائة|مائتان|ثلاثمائة|أربعمائة|خمسمائة|ستمائة|سبعمائة|ثمانمائة|تسعمائة)"
    ]
  , prod = \tokens -> case tokens of
      Token RegexMatch (GroupMatch (match:_)):_ -> case match of
        "مائة" -> integer 100
        "سبعمائة" -> integer 700
        "خمسمائة" -> integer 500
        "أربعمائة" -> integer 400
        "ستمائة" -> integer 600
        "مائتان" -> integer 200
        "ثلاثمائة" -> integer 300
        "ثمانمائة" -> integer 800
        "تسعمائة" -> integer 900
        _ -> Nothing
      _ -> Nothing
  }

ruleNumeralDotNumeral :: Rule
ruleNumeralDotNumeral = Rule
  { name = "number dot number"
  , pattern =
    [ dimension Numeral
    , regex "فاصلة"
    , numberWith TNumeral.grain isNothing
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = v1}):
       _:
       Token Numeral (NumeralData {TNumeral.value = v2}):
       _) -> double $ v1 + decimalsToDouble v2
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
        parseDouble (Text.replace (Text.singleton ',') Text.empty match) >>= double
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleDecimalNumeral
  , ruleDecimalWithThousandsSeparator
  , ruleInteger
  , ruleInteger11
  , ruleInteger12
  , ruleInteger13
  , ruleInteger14
  , ruleInteger15
  , ruleInteger18
  , ruleInteger19
  , ruleInteger2
  , ruleInteger20
  , ruleInteger21
  , ruleInteger22
  , ruleInteger23
  , ruleInteger3
  , ruleInteger4
  , ruleInteger5
  , ruleInteger7
  , ruleInteger9
  , ruleIntegerNumeric
  , ruleIntegerWithThousandsSeparator
  , ruleMultiply
  , ruleNumeralDotNumeral
  , ruleNumeralsPrefixWithMinus
  , rulePowersOfTen
  ]
