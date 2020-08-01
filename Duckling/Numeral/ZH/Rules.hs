-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.ZH.Rules
  ( rules
  ) where

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

ruleInteger :: Rule
ruleInteger = Rule
  { name = "integer (0..10)"
  , pattern =
    [ regex "(〇|零|一|二|两|兩|三|四|五|六|七|八|九|十)"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup match integerMap >>= integer
      _ -> Nothing
  }

integerMap :: HashMap.HashMap Text Integer
integerMap = HashMap.fromList
  [ ( "〇", 0 )
  , ( "零", 0 )
  , ( "一", 1 )
  , ( "兩", 2 )
  , ( "两", 2 )
  , ( "二", 2 )
  , ( "三", 3 )
  , ( "四", 4 )
  , ( "五", 5 )
  , ( "六", 6 )
  , ( "七", 7 )
  , ( "八", 8 )
  , ( "九", 9 )
  , ( "十", 10 )
  ]


ruleNumeralsPrefixWithNegativeOrMinus :: Rule
ruleNumeralsPrefixWithNegativeOrMinus = Rule
  { name = "numbers prefix with -, negative or minus"
  , pattern =
    [ regex "-|负|負"
    , Predicate isPositive
    ]
  , prod = \case
      (_:Token Numeral nd:_) -> double (TNumeral.value nd * (-1))
      _ -> Nothing
  }

ruleDecimalWithThousandsSeparator :: Rule
ruleDecimalWithThousandsSeparator = Rule
  { name = "decimal with thousands separator"
  , pattern =
    [ regex "(\\d+(,\\d\\d\\d)+\\.\\d+)"
    ]
  , prod = \case
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
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) -> parseDecimal True match
      _ -> Nothing
  }

ruleNumeral :: Rule
ruleNumeral = Rule
  { name = "<number>个/個"
  , pattern =
    [ dimension Numeral
    , regex "个|個"
    ]
  , prod = \case
      (token:_) -> Just token
      _ -> Nothing
  }

numeralSuffixList :: [(Text, Maybe Token)]
numeralSuffixList =
  [ ("K", double 1e3 >>= withGrain 3 >>= withMultipliable)
  , ("M", double 1e6 >>= withGrain 6 >>= withMultipliable)
  , ("G", double 1e9 >>= withGrain 9 >>= withMultipliable)
  , ("十", double 1e1 >>= withGrain 1 >>= withMultipliable)
  , ("百", double 1e2 >>= withGrain 2 >>= withMultipliable)
  , ("千", double 1e3 >>= withGrain 3 >>= withMultipliable)
  , ("万", double 1e4 >>= withGrain 4 >>= withMultipliable)
  , ("亿", double 1e8 >>= withGrain 8 >>= withMultipliable)
  ]

ruleNumeralSuffixes :: [Rule]
ruleNumeralSuffixes = uncurry constructNumeralSuffixRule <$> numeralSuffixList
  where
    constructNumeralSuffixRule :: Text -> Maybe Token -> Rule
    constructNumeralSuffixRule suffixName production = Rule
      { name = "number suffix: " `mappend` suffixName
      , pattern =
        [ regex $ Text.unpack suffixName
        ]
      , prod = const production
      }


ruleMultiply :: Rule
ruleMultiply = Rule
  { name = "compose by multiplication"
  , pattern =
    [ dimension Numeral
    , Predicate isMultipliable
    ]
  , prod = \case
      (token1:token2:_) -> multiply token1 token2
      _ -> Nothing
  }

ruleIntegerWithThousandsSeparator :: Rule
ruleIntegerWithThousandsSeparator = Rule
  { name = "integer with thousands separator ,"
  , pattern =
    [ regex "(\\d{1,3}(,\\d\\d\\d){1,5})"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):
       _) -> let fmt = Text.replace "," Text.empty match
        in parseDouble fmt >>= double
      _ -> Nothing
  }

ruleNumeralsIntersectNonconsectiveUnit :: Rule
ruleNumeralsIntersectNonconsectiveUnit = Rule
  { name = "integer with nonconsecutive unit modifiers"
  , pattern =
    [ Predicate isPositive
    , regex "零|〇"
    , Predicate isPositive
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = v1}:_:
       Token Numeral NumeralData{TNumeral.value = v2}:_) ->
        sumConnectedNumbers v1 v2 (diffIntegerDigits v1 v2)
        >>= double
      _ -> Nothing
  }
  where
    sumConnectedNumbers :: Double -> Double -> Int -> Maybe Double
    sumConnectedNumbers v1 v2 d
      | d <= 1 = Nothing
      | otherwise = Just $ v1 + v2

ruleNumeralsIntersectConsecutiveUnit :: Rule
ruleNumeralsIntersectConsecutiveUnit = Rule
  { name = "integer with consecutive unit modifiers"
  , pattern =
    [ Predicate isPositive
    , Predicate isPositive
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = v1}:
       Token Numeral NumeralData{TNumeral.value = v2}:_) ->
        sumConnectedNumbers v1 v2 (diffIntegerDigits v1 v2)
        >>= double
      _ -> Nothing
  }
  where
    sumConnectedNumbers :: Double -> Double -> Int -> Maybe Double
    sumConnectedNumbers v1 v2 d
      | d == 1 = Just $ v1 + v2
      | otherwise = Nothing

ruleHundredPrefix :: Rule
ruleHundredPrefix = Rule
  { name = "one hundred and <integer> (short form)"
  , pattern =
    [ regex "百|佰"
    , numberBetween 1 10
    ]
  , prod = \case
    (_:Token Numeral NumeralData{TNumeral.value =Just  v}:_) ->
      double $ 100 + v*10
    _ -> Nothing
  }

ruleThousandPrefix :: Rule
ruleThousandPrefix = Rule
  { name = "one thousand and <integer> (short form)"
  , pattern =
    [ regex "千|仟"
    , numberBetween 1 10
    ]
  , prod = \case
    (_:Token Numeral NumeralData{TNumeral.value = Just v}:_) ->
      double $ 1000 + v*100
    _ -> Nothing
  }

ruleTenThousandPrefix :: Rule
ruleTenThousandPrefix = Rule
  { name = "ten thousand and <integer> (short form)"
  , pattern =
    [ regex "万|萬"
    , numberBetween 1 10
    ]
  , prod = \case
    (_:Token Numeral NumeralData{TNumeral.value = Just v}:_) ->
      double $ 10000 + v*1000
    _ -> Nothing
  }

ruleIntervalNumeralDash :: Rule
ruleIntervalNumeralDash = Rule
  { name = "<numeral> - <numeral>"
  , pattern =
    [ Predicate isPositive
    , regex "-|~|到|至|或"
    , Predicate isSimpleNumeral
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = Just from}:
       _:
       Token Numeral NumeralData{TNumeral.value = Just to}:
       _) | from < to ->
         Just . Token Numeral $ withIntervalNum (from, to)
      _ -> Nothing
  }

ruleIntervalDash :: Rule
ruleIntervalDash = Rule
  { name = "<numeral> - <numeral>"
  , pattern =
    [ Predicate isSimpleNumeral
    , regex "-|~|到|至|或"
    , Predicate isSimpleNumeral
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = Just from}:
       _:
       Token Numeral NumeralData{TNumeral.value = Just to}:
       _) | from < to ->
        Just . Token Numeral $ withIntervalNum (from, to)
      _ -> Nothing
  }

ruleIntervalShort :: Rule
ruleIntervalShort = Rule
  { name = "<numeral> <numeral>"
  , pattern =
    [ Predicate isPositive
    , Predicate isPositive
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = Just from}:
       Token Numeral NumeralData{TNumeral.value = Just to}:
       _) | from < to && (to - from == 1 || floor(to-from) `mod` 10 == 0) && (diffIntegerDigits to from <= 1) ->
         Just . Token Numeral $ withIntervalNum (from, to)
      _ -> Nothing
  }

ruleInterval2To3 :: Rule
ruleInterval2To3 = Rule
  { name = "two to three (special)"
  , pattern =
    [ regex "三兩個?"
    ]
  , prod = \case
      (_:_) ->
         Just . Token Numeral $ withIntervalNum (2, 3)
      _ -> Nothing
  }

ruleIntervalShort2 :: Rule
ruleIntervalShort2 = Rule
  { name = "prefix <numeral>"
  , pattern =
    [ regex "(十|拾|百|佰|千|仟|万|萬)"
    , Predicate isNumeralInterval
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):
        Token Numeral NumeralData{TNumeral.minValue = Just from, 
        TNumeral.maxValue = Just to}:_) | (diffIntegerDigits to from) == 0 -> case match of
        "十" -> Just . Token Numeral $ withIntervalNum (10+from, 10+to)
        "拾" -> Just . Token Numeral $ withIntervalNum (10+from, 10+to)
        "百" -> Just . Token Numeral $ withIntervalNum (100+from*10, 100+to*10)
        "佰" -> Just . Token Numeral $ withIntervalNum (100+from*10, 100+to*10)
        "千" -> Just . Token Numeral $ withIntervalNum (1000+from*100, 1000+to*100)
        "仟" -> Just . Token Numeral $ withIntervalNum (1000+from*100, 1000+to*100)
        "万" -> Just . Token Numeral $ withIntervalNum (10000+from*1000, 10000+to*1000)
        "萬" -> Just . Token Numeral $ withIntervalNum (10000+from*1000, 10000+to*1000)
        _ -> Nothing
      _ -> Nothing
  }

ruleIntervalShort3 :: Rule
ruleIntervalShort3 = Rule
  { name = "<numeral> suffix"
  , pattern =
    [ Predicate isNumeralInterval
    , regex "(十|拾|百|佰|千|仟|万|萬)"
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.minValue = Just from, 
        TNumeral.maxValue = Just to}:
        Token RegexMatch (GroupMatch (match:_)):_) | (diffIntegerDigits to from) == 0 -> case match of
        "十" -> Just . Token Numeral $ withIntervalNum (10*from, 10*to)
        "拾" -> Just . Token Numeral $ withIntervalNum (10*from, 10*to)
        "百" -> Just . Token Numeral $ withIntervalNum (100*from, 100*to)
        "佰" -> Just . Token Numeral $ withIntervalNum (100*from, 100*to)
        "千" -> Just . Token Numeral $ withIntervalNum (1000*from, 1000*to)
        "仟" -> Just . Token Numeral $ withIntervalNum (1000*from, 1000*to)
        "万" -> Just . Token Numeral $ withIntervalNum (10000*from, 10000*to)
        "萬" -> Just . Token Numeral $ withIntervalNum (10000*from, 10000*to)
        _ -> Nothing
      _ -> Nothing
  }

ruleAFew :: Rule
ruleAFew = Rule
  { name = "a few"
  , pattern =
    [ regex "幾|數|若干"
    ]
  , prod = \case
      (token:_) -> Just . Token Numeral $ withIntervalNum (3, 9)
      _ -> Nothing
  }

ruleIntervalBound :: Rule
ruleIntervalBound = Rule
  { name = "under/less/lower/no more than <numeral> (最多|至少|最少)"
  , pattern =
    [ regex "(最多|至少|最少|起碼)"
    , Predicate isSimpleNumeral
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):
       Token Numeral NumeralData{TNumeral.value = Just to}:
       _) -> case match of
        "最多" -> Just . Token Numeral $ withMaxNum to
        "最少" -> Just . Token Numeral $ withMinNum to
        "至少" -> Just . Token Numeral $ withMinNum to
        "起碼" -> Just . Token Numeral $ withMinNum to
        _ -> Nothing
      _ -> Nothing
  }

ruleIntervalBound2 :: Rule
ruleIntervalBound2 = Rule
  { name = "under/less/lower/no more than <numeral> (以下|以上)"
  , pattern =
    [ Predicate isSimpleNumeral
    , regex "(以下|以上)"
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = Just to}:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> case match of
        "以下" -> Just . Token Numeral $ withMaxNum to
        "以上" -> Just . Token Numeral $ withMinNum to
        _ -> Nothing
      _ -> Nothing
  }

rulePrecision :: Rule
rulePrecision = Rule
  { name = "about <numeral>"
  , pattern =
    [ dimension Numeral
    , regex "左右"
    ]
  , prod = \case
      (token:_) -> Just token
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleDecimalNumeral
  , ruleDecimalWithThousandsSeparator
  , ruleInteger
  , ruleIntegerWithThousandsSeparator
  , ruleNumeral
  , ruleNumeralsIntersectConsecutiveUnit
  , ruleNumeralsIntersectNonconsectiveUnit
  , ruleNumeralsPrefixWithNegativeOrMinus
  , ruleMultiply
  , ruleTens
  , ruleCompositeTens
  , ruleHalf
  , ruleDotSpelledOut
  , ruleDozen
  , rulePair
  , ruleFraction
  , ruleMixedFraction
  , ruleHundredPrefix
  , ruleThousandPrefix
  , ruleTenThousandPrefix
  , ruleIntervalNumeralDash
  , ruleIntervalDash
  , ruleIntervalShort
  , ruleIntervalShort2
  , ruleIntervalShort3
  , ruleIntervalBound
  , ruleIntervalBound2
  , rulePrecision
  , ruleAFew
  , ruleInterval2To3
  ]
  ++ ruleNumeralSuffixes
