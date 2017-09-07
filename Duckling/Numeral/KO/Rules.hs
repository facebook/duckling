-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.KO.Rules
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

ruleIntegerForOrdinals :: Rule
ruleIntegerForOrdinals = Rule
  { name = "integer (1..4) - for ordinals"
  , pattern =
    [ regex "(한|첫|두|세|네)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case match of
        "한" -> integer 1
        "첫" -> integer 1
        "두" -> integer 2
        "세" -> integer 3
        "네" -> integer 4
        _ -> Nothing
      _ -> Nothing
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
        parseDouble (Text.replace (Text.singleton ',') Text.empty match) >>= double
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

ruleFraction2 :: Rule
ruleFraction2 = Rule
  { name = "fraction"
  , pattern =
    [ dimension Numeral
    , regex "/"
    , dimension Numeral
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = v1}):
       _:
       Token Numeral (NumeralData {TNumeral.value = v2}):
       _) -> double $ v1 / v2
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

ruleIntegerTypeAndOrdinals :: Rule
ruleIntegerTypeAndOrdinals = Rule
  { name = "integer (20..90) - TYPE 2 and ordinals"
  , pattern =
    [ regex "(열|스물|서른|마흔|쉰|예순|일흔|여든|아흔)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case match of
        "열" -> integer 10
        "스물" -> integer 20
        "서른" -> integer 30
        "마흔" -> integer 40
        "쉰" -> integer 50
        "예순" -> integer 60
        "일흔" -> integer 70
        "여든" -> integer 80
        "아흔" -> integer 90
        _ -> Nothing
      _ -> Nothing
  }

ruleIntegerType1 :: Rule
ruleIntegerType1 = Rule
  { name = "integer - TYPE 1"
  , pattern =
    [ regex "(영|일|이|삼|사|오|육|칠|팔|구)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case match of
        "영" -> integer 0
        "일" -> integer 1
        "이" -> integer 2
        "삼" -> integer 3
        "사" -> integer 4
        "오" -> integer 5
        "육" -> integer 6
        "칠" -> integer 7
        "팔" -> integer 8
        "구" -> integer 9
        _ -> Nothing
      _ -> Nothing
  }

ruleIntegerType1PowersOfTen :: Rule
ruleIntegerType1PowersOfTen = Rule
  { name = "integer - TYPE 1: powers of ten"
  , pattern =
    [ regex "(십|백|천|만|억|조)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case match of
        "십" -> double 10   >>= withGrain 1  >>= withMultipliable
        "백" -> double 1e2  >>= withGrain 2  >>= withMultipliable
        "천" -> double 1e3  >>= withGrain 3  >>= withMultipliable
        "만" -> double 1e4  >>= withGrain 4  >>= withMultipliable
        "억" -> double 1e8  >>= withGrain 8  >>= withMultipliable
        "조" -> double 1e12 >>= withGrain 12 >>= withMultipliable
        _ -> Nothing
      _ -> Nothing
  }

ruleSum :: Rule
ruleSum = Rule
  { name = "intersect 2 numbers"
  , pattern =
    [ numberWith (fromMaybe 0 . TNumeral.grain) (>1)
    , numberWith TNumeral.multipliable not
    ]
  , prod = \tokens ->
      case tokens of
        (Token Numeral (NumeralData {TNumeral.value = val1, TNumeral.grain = Just g}):
         Token Numeral (NumeralData {TNumeral.value = val2}):
         _) | (10 ** fromIntegral g) > val2 -> double $ val1 + val2
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

ruleIntegerType2 :: Rule
ruleIntegerType2 = Rule
  { name = "integer (1..10) - TYPE 2"
  , pattern =
    [ regex "(하나|둘|셋|넷|다섯|여섯|일곱|여덟|아홉)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case match of
        "하나" -> integer 1
        "둘" -> integer 2
        "셋" -> integer 3
        "넷" -> integer 4
        "다섯" -> integer 5
        "여섯" -> integer 6
        "일곱" -> integer 7
        "여덟" -> integer 8
        "아홉" -> integer 9
        _ -> Nothing
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
      (Token Numeral (NumeralData {TNumeral.value = v1}):
       _:
       Token Numeral (NumeralData {TNumeral.value = v2}):
       _) -> double $ v2 / v1
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
      (Token Numeral (NumeralData {TNumeral.value = v1}):
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
      (Token Numeral (NumeralData {TNumeral.value = v1}):
       Token Numeral (NumeralData {TNumeral.value = v2}):
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
        parseDouble (Text.replace (Text.singleton ',') Text.empty match) >>= double
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleDecimalNumeral
  , ruleDecimalWithThousandsSeparator
  , ruleFew
  , ruleFraction
  , ruleFraction2
  , ruleHalf
  , ruleInteger
  , ruleIntegerForOrdinals
  , ruleIntegerNumeric
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
