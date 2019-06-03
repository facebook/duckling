-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.KO.Rules
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

integerForOrdinalsMap :: HashMap Text Integer
integerForOrdinalsMap = HashMap.fromList
  [ ( "한", 1 )
  , ( "첫", 1 )
  , ( "두", 2 )
  , ( "세", 3 )
  , ( "네", 4 )
  ]

ruleIntegerForOrdinals :: Rule
ruleIntegerForOrdinals = Rule
  { name = "integer (1..4) - for ordinals"
  , pattern =
    [ regex "(한|첫|두|세|네)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup match integerForOrdinalsMap >>= integer
      _ -> Nothing
  }

ruleFew :: Rule
ruleFew = Rule
  { name = "few 몇"
  , pattern =
    [ regex "몇"
    ]
  , prod = \_ -> integer 3
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

ruleFraction :: Rule
ruleFraction = Rule
  { name = "fraction"
  , pattern =
    [ dimension Numeral
    , regex "분(의|에)"
    , dimension Numeral
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v1}:
       _:
       Token Numeral NumeralData{TNumeral.value = v2}:
       _) -> double $ v2 / v1
      _ -> Nothing
  }

ruleNumeralsPrefixWithOr :: Rule
ruleNumeralsPrefixWithOr = Rule
  { name = "numbers prefix with -, 마이너스, or 마이나스"
  , pattern =
    [ regex "-|마이너스\\s?|마이나스\\s?"
    , dimension Numeral
    ]
  , prod = \tokens -> case tokens of
      (_:Token Numeral nd:_) -> double (TNumeral.value nd * (-1))
      _ -> Nothing
  }

ruleHalf :: Rule
ruleHalf = Rule
  { name = "half - 반"
  , pattern =
    [ regex "반"
    ]
  , prod = \_ -> double 0.5
  }

ruleInteger :: Rule
ruleInteger = Rule
  { name = "integer 0"
  , pattern =
    [ regex "영|공|빵"
    ]
  , prod = \_ -> integer 0
  }

integerTypeAndOrdinalsMap :: HashMap Text Integer
integerTypeAndOrdinalsMap = HashMap.fromList
  [ ( "열", 10 )
  , ( "스물", 20 )
  , ( "서른", 30 )
  , ( "마흔", 40 )
  , ( "쉰", 50 )
  , ( "예순", 60 )
  , ( "일흔", 70 )
  , ( "여든", 80 )
  , ( "아흔", 90 )
  ]

ruleIntegerTypeAndOrdinals :: Rule
ruleIntegerTypeAndOrdinals = Rule
  { name = "integer (20..90) - TYPE 2 and ordinals"
  , pattern =
    [ regex "(열|스물|서른|마흔|쉰|예순|일흔|여든|아흔)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup match integerTypeAndOrdinalsMap >>= integer
      _ -> Nothing
  }

integerType1Map :: HashMap Text Integer
integerType1Map = HashMap.fromList
  [ ( "영", 0 )
  , ( "일", 1 )
  , ( "이", 2 )
  , ( "삼", 3 )
  , ( "사", 4 )
  , ( "오", 5 )
  , ( "육", 6 )
  , ( "칠", 7 )
  , ( "팔", 8 )
  , ( "구", 9 )
  ]

ruleIntegerType1 :: Rule
ruleIntegerType1 = Rule
  { name = "integer - TYPE 1"
  , pattern =
    [ regex "(영|일|이|삼|사|오|육|칠|팔|구)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup match integerType1Map >>= integer
      _ -> Nothing
  }

integerType1PowersOfTenMap :: HashMap Text (Double, Int)
integerType1PowersOfTenMap = HashMap.fromList
  [ ( "십", (10, 1) )
  , ( "백", (1e2, 2) )
  , ( "천", (1e3, 3) )
  , ( "만", (1e4, 4) )
  , ( "억", (1e8, 8) )
  , ( "조", (1e12, 12) )
  ]

ruleIntegerType1PowersOfTen :: Rule
ruleIntegerType1PowersOfTen = Rule
  { name = "integer - TYPE 1: powers of ten"
  , pattern =
    [ regex "(십|백|천|만|억|조)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        do
          (value, grain) <- HashMap.lookup match integerType1PowersOfTenMap
          double value >>= withGrain grain >>= withMultipliable
      _ -> Nothing
  }

ruleSum :: Rule
ruleSum = Rule
  { name = "intersect 2 numbers"
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

ruleMultiply :: Rule
ruleMultiply = Rule
  { name = "compose by multiplication"
  , pattern =
    [ dimension Numeral
    , Predicate isMultipliable
    ]
  , prod = \tokens -> case tokens of
      (token1:token2:_) -> multiply token1 token2
      _ -> Nothing
  }

integerType2Map :: HashMap Text Integer
integerType2Map = HashMap.fromList
  [ ( "하나", 1 )
  , ( "둘", 2 )
  , ( "셋", 3 )
  , ( "넷", 4 )
  , ( "다섯", 5 )
  , ( "여섯", 6 )
  , ( "일곱", 7 )
  , ( "여덟", 8 )
  , ( "아홉", 9 )
  ]

ruleIntegerType2 :: Rule
ruleIntegerType2 = Rule
  { name = "integer (1..10) - TYPE 2"
  , pattern =
    [ regex "(하나|둘|셋|넷|다섯|여섯|일곱|여덟|아홉)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup match integerType2Map >>= integer
      _ -> Nothing
  }

ruleNumeralDotNumeral :: Rule
ruleNumeralDotNumeral = Rule
  { name = "number dot number - 삼점사"
  , pattern =
    [ dimension Numeral
    , regex "(점|쩜)((영|일|이|삼|사|오|육|칠|팔|구)+)"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v1}:
       Token RegexMatch (GroupMatch (_:match:_)):
       _) -> do
        let getDigit '영' = Just "0"
            getDigit '일' = Just "1"
            getDigit '이' = Just "2"
            getDigit '삼' = Just "3"
            getDigit '사' = Just "4"
            getDigit '오' = Just "5"
            getDigit '육' = Just "6"
            getDigit '칠' = Just "7"
            getDigit '팔' = Just "8"
            getDigit '구' = Just "9"
            getDigit _ = Nothing
        v2 <- parseDouble . Text.concat . mapMaybe getDigit $ Text.unpack match
        double $ v1 + decimalsToDouble v2
      _ -> Nothing
  }

ruleIntegerType3 :: Rule
ruleIntegerType3 = Rule
  { name = "integer (21..99) - TYPE 2"
  , pattern =
    [ oneOf [10, 20 .. 90]
    , oneOf [1 .. 9]
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v1}:
       Token Numeral NumeralData{TNumeral.value = v2}:
       _) -> double $ v1 + v2
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
  , ruleFew
  , ruleFraction
  , ruleHalf
  , ruleInteger
  , ruleIntegerForOrdinals
  , ruleIntegerType1
  , ruleIntegerType1PowersOfTen
  , ruleSum
  , ruleMultiply
  , ruleIntegerType2
  , ruleIntegerType3
  , ruleIntegerTypeAndOrdinals
  , ruleIntegerWithThousandsSeparator
  , ruleNumeralDotNumeral
  , ruleNumeralsPrefixWithOr
  ]
