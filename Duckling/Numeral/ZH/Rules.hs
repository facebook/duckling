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
    [ regex "(〇|零|一|二|两|兩|三|四|五|六|七|八|九|十|壹|貳|參|肆|伍|陸|柒|捌|玖|拾)"
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
  , ( "壹", 1 )
  , ( "兩", 2 )
  , ( "两", 2 )
  , ( "二", 2 )
  , ( "貳", 2 )
  , ( "三", 3 )
  , ( "參", 3 )
  , ( "四", 4 )
  , ( "肆", 4 )
  , ( "五", 5 )
  , ( "伍", 5 )
  , ( "六", 6 )
  , ( "陸", 6 )
  , ( "七", 7 )
  , ( "柒", 7 )
  , ( "八", 8 )
  , ( "捌", 8 )
  , ( "九", 9 )
  , ( "玖", 9 )
  , ( "十", 10 )
  , ( "拾", 10 )
  ]

tensMap :: HashMap.HashMap Text Integer
tensMap = HashMap.fromList
  [ ( "廿" , 20 )
  , ( "卅" , 30 )
  , ( "卌" , 40 )
  ]

ruleTens :: Rule
ruleTens = Rule
  { name = "integer (20,30,40)"
  , pattern =
    [ regex "(廿|卅|卌)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) tensMap >>= integer
      _ -> Nothing
  }

ruleCompositeTens :: Rule
ruleCompositeTens = Rule
  { name = "integer 21..49"
  , pattern =
    [ oneOf [20,30,40]
    , regex "[\\s\\-]+"
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

ruleDotSpelledOut :: Rule
ruleDotSpelledOut = Rule
  { name = "one point 2"
  , pattern =
    [ dimension Numeral
    , regex "點"
    , Predicate $ not . hasGrain
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral nd1:_:Token Numeral nd2:_) ->
        double $ TNumeral.value nd1 + decimalsToDouble (TNumeral.value nd2)
      _ -> Nothing
  }

ruleFraction :: Rule
ruleFraction = Rule
  { name = "fraction"
  , pattern =
    [ dimension Numeral
    , regex "分之|分|份"
    , dimension Numeral
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v1}:
       _:
       Token Numeral NumeralData{TNumeral.value = v2}:
       _) -> double $ v2 / v1
      _ -> Nothing
  }

ruleMixedFraction :: Rule
ruleMixedFraction = Rule
  { name = " mixed fraction"
  , pattern =
    [ dimension Numeral
    , regex "又"
    , dimension Numeral
    , regex "分之|分|份"
    , dimension Numeral
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v1}:
       _:
       Token Numeral NumeralData{TNumeral.value = v2}:
       _:
       Token Numeral NumeralData{TNumeral.value = v3}:
       _) -> double $ v3 / v2 + v1
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

ruleHalf :: Rule
ruleHalf = Rule
  { name = "half"
  , pattern =
    [ regex "(1|一)?半(半|个|個)?"
    ]
  , prod = \case
      (_:_) -> double 0.5 >>= withMultipliable
      _ -> Nothing
  }

ruleDozen :: Rule
ruleDozen = Rule
  { name = "a dozen of"
  , pattern =
    [ regex "打"
    ]
  , prod = \_ -> integer 12 >>= withMultipliable >>= notOkForAnyTime
  }

rulePair :: Rule
rulePair = Rule
  { name = "a pair"
  , pattern =
    [ regex "雙|對"
    ]
  , prod = \_ -> integer 2 >>= withMultipliable >>= notOkForAnyTime
  }

numeralSuffixList :: [(Text, Maybe Token)]
numeralSuffixList =
  [ ("K", double 1e3 >>= withGrain 3 >>= withMultipliable)
  , ("M", double 1e6 >>= withGrain 6 >>= withMultipliable)
  , ("G", double 1e9 >>= withGrain 9 >>= withMultipliable)
  , ("十|拾", double 1e1 >>= withGrain 1 >>= withMultipliable)
  , ("百|佰", double 1e2 >>= withGrain 2 >>= withMultipliable)
  , ("千|仟", double 1e3 >>= withGrain 3 >>= withMultipliable)
  , ("万|萬", double 1e4 >>= withGrain 4 >>= withMultipliable)
  , ("亿|億", double 1e8 >>= withGrain 8 >>= withMultipliable)
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
    (_:Token Numeral NumeralData{TNumeral.value = v}:_) ->
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
    (_:Token Numeral NumeralData{TNumeral.value = v}:_) ->
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
    (_:Token Numeral NumeralData{TNumeral.value = v}:_) ->
      double $ 10000 + v*1000
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
  ]
  ++ ruleNumeralSuffixes
