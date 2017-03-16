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
    [ regex "(\xd55c|\xccab|\xb450|\xc138|\xb124)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case match of
        "\xd55c" -> integer 1
        "\xccab" -> integer 1
        "\xb450" -> integer 2
        "\xc138" -> integer 3
        "\xb124" -> integer 4
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
    [ regex "\xba87"
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
    [ regex "-|\xb9c8\xc774\xb108\xc2a4\\s?|\xb9c8\xc774\xb098\xc2a4\\s?"
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
    [ regex "\xbc18"
    ]
  , prod = \_ -> double 0.5
  }

ruleInteger :: Rule
ruleInteger = Rule
  { name = "integer 0"
  , pattern =
    [ regex "\xc601|\xacf5|\xbe75"
    ]
  , prod = \_ -> integer 0
  }

ruleIntegerTypeAndOrdinals :: Rule
ruleIntegerTypeAndOrdinals = Rule
  { name = "integer (20..90) - TYPE 2 and ordinals"
  , pattern =
    [ regex "(\xc5f4|\xc2a4\xbb3c|\xc11c\xb978|\xb9c8\xd754|\xc270|\xc608\xc21c|\xc77c\xd754|\xc5ec\xb4e0|\xc544\xd754)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case match of
        "\xc5f4" -> integer 10
        "\xc2a4\xbb3c" -> integer 20
        "\xc11c\xb978" -> integer 30
        "\xb9c8\xd754" -> integer 40
        "\xc270" -> integer 50
        "\xc608\xc21c" -> integer 60
        "\xc77c\xd754" -> integer 70
        "\xc5ec\xb4e0" -> integer 80
        "\xc544\xd754" -> integer 90
        _ -> Nothing
      _ -> Nothing
  }

ruleIntegerType1 :: Rule
ruleIntegerType1 = Rule
  { name = "integer - TYPE 1"
  , pattern =
    [ regex "(\xc601|\xc77c|\xc774|\xc0bc|\xc0ac|\xc624|\xc721|\xce60|\xd314|\xad6c)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case match of
        "\xc601" -> integer 0
        "\xc77c" -> integer 1
        "\xc774" -> integer 2
        "\xc0bc" -> integer 3
        "\xc0ac" -> integer 4
        "\xc624" -> integer 5
        "\xc721" -> integer 6
        "\xce60" -> integer 7
        "\xd314" -> integer 8
        "\xad6c" -> integer 9
        _ -> Nothing
      _ -> Nothing
  }

ruleIntegerType1PowersOfTen :: Rule
ruleIntegerType1PowersOfTen = Rule
  { name = "integer - TYPE 1: powers of ten"
  , pattern =
    [ regex "(\xc2ed|\xbc31|\xcc9c|\xb9cc|\xc5b5|\xc870)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case match of
        "\xc2ed" -> double 10   >>= withGrain 1  >>= withMultipliable
        "\xbc31" -> double 1e2  >>= withGrain 2  >>= withMultipliable
        "\xcc9c" -> double 1e3  >>= withGrain 3  >>= withMultipliable
        "\xb9cc" -> double 1e4  >>= withGrain 4  >>= withMultipliable
        "\xc5b5" -> double 1e8  >>= withGrain 8  >>= withMultipliable
        "\xc870" -> double 1e12 >>= withGrain 12 >>= withMultipliable
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
    [ regex "(\xd558\xb098|\xb458|\xc14b|\xb137|\xb2e4\xc12f|\xc5ec\xc12f|\xc77c\xacf1|\xc5ec\xb35f|\xc544\xd649)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case match of
        "\xd558\xb098" -> integer 1
        "\xb458" -> integer 2
        "\xc14b" -> integer 3
        "\xb137" -> integer 4
        "\xb2e4\xc12f" -> integer 5
        "\xc5ec\xc12f" -> integer 6
        "\xc77c\xacf1" -> integer 7
        "\xc5ec\xb35f" -> integer 8
        "\xc544\xd649" -> integer 9
        _ -> Nothing
      _ -> Nothing
  }

ruleFraction :: Rule
ruleFraction = Rule
  { name = "fraction"
  , pattern =
    [ dimension Numeral
    , regex "\xbd84(\xc758|\xc5d0)"
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
    , regex "(\xc810|\xca5c)((\xc601|\xc77c|\xc774|\xc0bc|\xc0ac|\xc624|\xc721|\xce60|\xd314|\xad6c)+)"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = v1}):
       Token RegexMatch (GroupMatch (_:match:_)):
       _) -> do
        let getDigit '\xc601' = Just "0"
            getDigit '\xc77c' = Just "1"
            getDigit '\xc774' = Just "2"
            getDigit '\xc0bc' = Just "3"
            getDigit '\xc0ac' = Just "4"
            getDigit '\xc624' = Just "5"
            getDigit '\xc721' = Just "6"
            getDigit '\xce60' = Just "7"
            getDigit '\xd314' = Just "8"
            getDigit '\xad6c' = Just "9"
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
