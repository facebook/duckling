-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.VI.Rules
  ( rules
  ) where

import Data.Maybe
import Data.String
import Prelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Regex.Types
import Duckling.Types
import qualified Duckling.Numeral.Types as TNumeral

powersOfTenMap :: HashMap.HashMap Text.Text (Double, Int)
powersOfTenMap = HashMap.fromList
  [ ( "chục",  (1e1, 1) )
  , ( "trăm",  (1e2, 2) )
  , ( "nghìn", (1e3, 3) )
  , ( "ngàn",  (1e3, 3) )
  , ( "triệu", (1e6, 6) )
  , ( "tỷ",    (1e9, 9) )
  , ( "tỉ",    (1e9, 9) )
  ]

rulePowersOfTen :: Rule
rulePowersOfTen = Rule
  { name = "powers of tens"
  , pattern =
    [ regex "(chục|trăm|nghìn|ngàn|triệu|t(ỷ|ỉ))"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        (value, grain) <- HashMap.lookup (Text.toLower match) powersOfTenMap
        double value >>= withGrain grain >>= withMultipliable
      _ -> Nothing
  }

ruleNumeralsPrefixWithM :: Rule
ruleNumeralsPrefixWithM = Rule
  { name = "numbers prefix with -, âm"
  , pattern =
    [ regex "\\-|âm"
    , Predicate isPositive
    ]
  , prod = \tokens -> case tokens of
      (_:Token Numeral nd:_) -> double (TNumeral.value nd * (-1))
      _ -> Nothing
  }

ruleNumerals2 :: Rule
ruleNumerals2 = Rule
  { name = "numbers 25 35 45 55 65 75 85 95"
  , pattern =
    [ oneOf [20, 30 .. 90]
    , regex "lăm"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v}:_) -> double $ v + 5
      _ -> Nothing
  }

ruleDecimalWithThousandsSeparator :: Rule
ruleDecimalWithThousandsSeparator = Rule
  { name = "decimal with thousands separator"
  , pattern =
    [ regex "(\\d+(\\.\\d\\d\\d)+,\\d+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        parseDouble (Text.replace "." Text.empty match) >>= double
      _ -> Nothing
  }

ruleDecimalNumeral :: Rule
ruleDecimalNumeral = Rule
  { name = "decimal number"
  , pattern =
    [ regex "(\\d*,\\d+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> parseDecimal False match
      _ -> Nothing
  }

ruleInteger3 :: Rule
ruleInteger3 = Rule
  { name = "integer 21..99"
  , pattern =
    [ oneOf [20, 30 .. 90]
    , numberBetween 1 10
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v1}:
       Token Numeral NumeralData{TNumeral.value = v2}:
       _) -> double $ v1 + v2
      _ -> Nothing
  }

ruleNumeralDot :: Rule
ruleNumeralDot = Rule
  { name = "number dot 1 9"
  , pattern =
    [ dimension Numeral
    , regex "chấm|phẩy"
    , Predicate $ not . hasGrain
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral nd1:_:Token Numeral nd2:_) ->
        double $ TNumeral.value nd1 + decimalsToDouble (TNumeral.value nd2)
      _ -> Nothing
  }

ruleIntersect :: Rule
ruleIntersect = Rule
  { name = "intersect"
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

ruleNumeralsSuffixesKMG :: Rule
ruleNumeralsSuffixesKMG = Rule
  { name = "numbers suffixes (K, M, G)"
  , pattern =
    [ dimension Numeral
    , regex "([kmg])(?=[\\W\\$€]|$)"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v}:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> case Text.toLower match of
         "k" -> double $ v * 1e3
         "m" -> double $ v * 1e6
         "g" -> double $ v * 1e9
         _   -> Nothing
      _ -> Nothing
  }

integerMap :: HashMap.HashMap Text.Text Integer
integerMap = HashMap.fromList
  [ ("không", 0)
  , ("một", 1)
  , ("linh một", 1)
  , ("lẻ một", 1)
  , ("hai", 2)
  , ("lẻ hai", 2)
  , ("linh hai", 2)
  , ("ba", 3)
  , ("lẻ", 3)
  , ("linh ba", 3)
  , ("lẻ bốn", 4)
  , ("linh bốn", 4)
  , ("bốn", 4)
  , ("năm", 5)
  , ("lẻ năm", 5)
  , ("linh năm", 5)
  , ("linh sáu", 6)
  , ("sáu", 6)
  , ("lẻ sáu", 6)
  , ("linh bảy", 7)
  , ("lẻ bảy", 7)
  , ("bảy", 7)
  , ("lẻ tám", 8)
  , ("linh tám", 8)
  , ("tám", 8)
  , ("lẻ chín", 9)
  , ("chín", 9)
  , ("linh chín", 9)
  , ("linh mười", 10)
  , ("mười", 10)
  , ("lẻ mười", 10)
  , ("mười một", 11)
  , ("mười hai", 12)
  , ("mười ba", 13)
  , ("mười bốn", 14)
  , ("mười lăm", 15)
  , ("mười sáu", 16)
  , ("mười bảy", 17)
  , ("mười tám", 18)
  , ("mười chín", 19)
  ]

ruleInteger :: Rule
ruleInteger = Rule
  { name = "integer (0..19)"
  , pattern =
    [ regex "(không|một|linh một|lẻ một|hai|linh hai|lẻ hai|ba|linh ba|lẻ ba|bốn|linh bốn|lẻ bốn|năm|linh năm|lẻ năm|sáu|lẻ sáu|linh sáu|bảy|lẻ bảy|linh bảy|tám|linh tám|lẻ tám|chín|linh chín|lẻ chín|mười một|mười hai|mười ba|mười bốn|mười lăm|mười sáu|mười bảy|mười tám|mười chín|mười|linh mười)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) integerMap >>= integer
      _ -> Nothing
  }

tensMap :: HashMap.HashMap Text.Text Integer
tensMap = HashMap.fromList
  [ ("hai mươi", 20)
  , ("ba mươi", 30)
  , ("bốn mươi", 40)
  , ("năm mươi", 50)
  , ("sáu mươi", 60)
  , ("bảy mươi", 70)
  , ("tám mươi", 80)
  , ("chín mươi", 90)
  ]

ruleInteger2 :: Rule
ruleInteger2 = Rule
  { name = "integer (20..90)"
  , pattern =
    [ regex "(hai mươi|ba mươi|bốn mươi|năm mươi|sáu mươi|bảy mươi|tám mươi|chín mươi)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) tensMap >>= integer
      _ -> Nothing
  }

ruleNumerals :: Rule
ruleNumerals = Rule
  { name = "numbers 21 31 41 51 61 71 81 91"
  , pattern =
    [ oneOf [20, 30 .. 90]
    , regex "mốt"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v}:_) -> double $ v + 1
      _ -> Nothing
  }

ruleT :: Rule
ruleT = Rule
  { name = "tá"
  , pattern =
    [ regex "tá"
    ]
  , prod = \_ -> integer 12 >>= withGrain 1 >>= withMultipliable
  }

ruleIntegerWithThousandsSeparator :: Rule
ruleIntegerWithThousandsSeparator = Rule
  { name = "integer with thousands separator ,"
  , pattern =
    [ regex "(\\d{1,3}(\\.\\d\\d\\d){1,5})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):
       _) -> let fmt = Text.replace "." Text.empty match
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
  , ruleIntegerWithThousandsSeparator
  , ruleIntersect
  , ruleMultiply
  , ruleNumeralDot
  , ruleNumerals
  , ruleNumerals2
  , ruleNumeralsPrefixWithM
  , ruleNumeralsSuffixesKMG
  , rulePowersOfTen
  , ruleT
  ]
