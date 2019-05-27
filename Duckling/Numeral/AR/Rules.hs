-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Numeral.AR.Rules (rules) where
import Data.Maybe
import Prelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Numeral.AR.Helpers
  ( digitsMap
  , parseArabicDoubleFromText
  , parseArabicIntegerFromText
  )
import Duckling.Numeral.Helpers
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Regex.Types
import Duckling.Types
import qualified Duckling.Numeral.Types as TNumeral

ruleInteger5 :: Rule
ruleInteger5 = Rule
  { name = "integer 4"
  , pattern =
    [ regex "([أا]ربع[ةه]?)"
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
      (Token Numeral NumeralData{TNumeral.value = v1}:
       _:
       Token Numeral NumeralData{TNumeral.value = v2}:
       _) -> double $ v1 + v2
      _ -> Nothing
  }

ruleInteger18 :: Rule
ruleInteger18 = Rule
  { name = "integer 12"
  , pattern =
    [ regex "([إا]?ثن(ت)?[يىا] ?عشر[ةه]?)"
    ]
  , prod = \_ -> integer 12
  }

ruleInteger19 :: Rule
ruleInteger19 = Rule
  { name = "integer (20..90)"
  , pattern =
    [ regex "(عشر|ثلاث|[أا]ربع|خمس|ست|سبع|ثمان|تسع)(ون|ين)"
    ]
  , prod = \tokens -> case tokens of
      Token RegexMatch (GroupMatch (match:_)):_ ->
        (* 10) <$> HashMap.lookup match digitsMap >>= integer
      _ -> Nothing
  }

ruleInteger200 :: Rule
ruleInteger200 = Rule
  { name = "integer (200)"
  , pattern =
    [ regex "مائتان|مائتين"
    ]
  , prod = const $ integer 200
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
      (Token Numeral NumeralData{TNumeral.value = v1}:
       _:
       Token Numeral NumeralData{TNumeral.value = v2}:
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
      (Token Numeral NumeralData{TNumeral.value = v}:_) -> double $ v + 10
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
    [ regex "([إاأ]حد[يى]? عشر[ةه]?)"
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
    [ regex "(ما?[ئي][ةه]|مئت(ان|ين)|مئات|[أا]لف(ان|ين)?|[آا]لاف|ملايين|مليون(ان|ين)?)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "مئة" ->
          double 1e2 >>= withGrain 2 >>= withMultipliable
        "مئه" ->
          double 1e2 >>= withGrain 2 >>= withMultipliable
        "مائة" ->
          double 1e2 >>= withGrain 2 >>= withMultipliable
        "مائه" ->
          double 1e2 >>= withGrain 2 >>= withMultipliable
        "مئتين" ->
          double 2e2 >>= withGrain 2
        "مئتان" ->
          double 2e2 >>= withGrain 2
        "مئات" ->
          double 1e2 >>= withGrain 2 >>= withMultipliable
        "ألف" -> double 1e3 >>= withGrain 3 >>= withMultipliable
        "الف" -> double 1e3 >>= withGrain 3 >>= withMultipliable
        "الفين" -> double 2e3 >>= withGrain 3
        "الفان" -> double 2e3 >>= withGrain 3
        "الاف" ->
          double 1e3 >>= withGrain 3 >>= withMultipliable
        "آلاف" ->
          double 1e3 >>= withGrain 3 >>= withMultipliable
        "ملايين" ->
          double 1e6 >>= withGrain 6 >>= withMultipliable
        "مليون" ->
          double 1e6 >>= withGrain 6 >>= withMultipliable
        "مليونين" ->
          double 2e6 >>= withGrain 6
        "مليونان" ->
          double 2e6 >>= withGrain 6
        _ -> Nothing
      _ -> Nothing
  }

ruleInteger3 :: Rule
ruleInteger3 = Rule
  { name = "integer 2"
  , pattern =
    [ regex "[إا]ثنت?[اي]ن"
    ]
  , prod = \_ -> integer 2
  }

ruleInteger13 :: Rule
ruleInteger13 = Rule
  { name = "integer 9"
  , pattern =
    [ regex "تسع[ةه]?"
    ]
  , prod = \_ -> integer 9
  }

ruleInteger12 :: Rule
ruleInteger12 = Rule
  { name = "integer 8"
  , pattern =
    [ regex "ثما??ني?[ةه]?"
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
      (_:Token Numeral NumeralData{TNumeral.value = v}:_) ->
        double (v * (- 1))
      _ -> Nothing
  }

ruleInteger7 :: Rule
ruleInteger7 = Rule
  { name = "integer 5"
  , pattern =
    [ regex "خمس[ةه]?"
    ]
  , prod = \_ -> integer 5
  }

ruleInteger14 :: Rule
ruleInteger14 = Rule
  { name = "integer 10"
  , pattern =
    [ regex "عشر[ةه]?"
    ]
  , prod = \_ -> integer 10
  }

ruleInteger9 :: Rule
ruleInteger9 = Rule
  { name = "integer 6"
  , pattern =
    [ regex "ست[ةه]?"
    ]
  , prod = \_ -> integer 6
  }

ruleInteger :: Rule
ruleInteger = Rule
  { name = "integer 0"
  , pattern =
    [ regex "صفر"
    ]
  , prod = \_ -> integer 0
  }

ruleInteger4 :: Rule
ruleInteger4 = Rule
  { name = "integer 3"
  , pattern =
    [ regex "(ثلاث[ةه]?)"
    ]
  , prod = \_ -> integer 3
  }

ruleInteger2 :: Rule
ruleInteger2 = Rule
  { name = "integer 1"
  , pattern =
    [ regex "واحد[ةه]?"
    ]
  , prod = \_ -> integer 1
  }

ruleInteger11 :: Rule
ruleInteger11 = Rule
  { name = "integer 7"
  , pattern =
    [ regex "سبع[ةه]?"
    ]
  , prod = \_ -> integer 7
  }

ruleNumeralDotNumeral :: Rule
ruleNumeralDotNumeral = Rule
  { name = "number dot number"
  , pattern =
    [ dimension Numeral
    , regex "فاصل[ةه]"
    , Predicate $ not . hasGrain
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v1}:
       _:
       Token Numeral NumeralData{TNumeral.value = v2}:
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
        parseDouble (Text.replace "," Text.empty match) >>= double
      _ -> Nothing
  }

ruleIntegerNumeric :: Rule
ruleIntegerNumeric = Rule
  { name = "Arabic integer numeric"
  , pattern =
    [ regex "([٠-٩]{1,18})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        parseArabicIntegerFromText match >>= integer
      _ -> Nothing
  }

ruleFractionsNumeric :: Rule
ruleFractionsNumeric = Rule
  { name = "Arabic fractional number numeric"
  , pattern =
    [ regex "([٠-٩]+)/([٠-٩]+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (numerator:denominator:_)):_) -> do
        n <- parseArabicIntegerFromText numerator >>= integer
        d <- parseArabicIntegerFromText denominator >>= integer
        divide n d >>= notOkForAnyTime
      _ -> Nothing
  }

ruleArabicDecimal :: Rule
ruleArabicDecimal = Rule
  { name = "Arabic decimal number with Arabic decimal separator"
  , pattern =
    [ regex "([٠-٩]*٫[٠-٩]+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        parseArabicDoubleFromText match >>= double
      _ -> Nothing
  }

ruleArabicCommas :: Rule
ruleArabicCommas = Rule
  { name = "Arabic number with commas and optional Arabic decimal separator"
  , pattern =
    [ regex "([٠-٩]{1,3}(٬[٠-٩]{3}){1,5}(٫[٠-٩]+)?)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        parseArabicDoubleFromText (Text.replace "٬" Text.empty match) >>=
          double
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleIntegerNumeric
  , ruleFractionsNumeric
  , ruleDecimalNumeral
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
  , ruleInteger21
  , ruleInteger22
  , ruleInteger23
  , ruleInteger3
  , ruleInteger4
  , ruleInteger5
  , ruleInteger7
  , ruleInteger9
  , ruleIntegerWithThousandsSeparator
  , ruleMultiply
  , ruleNumeralDotNumeral
  , ruleNumeralsPrefixWithMinus
  , rulePowersOfTen
  , ruleInteger200
  , ruleArabicDecimal
  , ruleArabicCommas
  ]
