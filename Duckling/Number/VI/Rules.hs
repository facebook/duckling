-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Number.VI.Rules
  ( rules ) where

import Data.Maybe
import qualified Data.Text as Text
import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Number.Helpers
import Duckling.Number.Types (NumberData (..))
import qualified Duckling.Number.Types as TNumber
import Duckling.Regex.Types
import Duckling.Types

rulePowersOfTen :: Rule
rulePowersOfTen = Rule
  { name = "powers of tens"
  , pattern =
    [ regex "(tr\x0103m?|ngh\x00ecn?|tri\x1ec7u?|t\x1ef7?)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "tr\x0103"   -> double 1e2 >>= withGrain 2 >>= withMultipliable
        "tr\x0103m"  -> double 1e2 >>= withGrain 2 >>= withMultipliable
        "ngh\x00ec"  -> double 1e3 >>= withGrain 3 >>= withMultipliable
        "ngh\x00ecn" -> double 1e3 >>= withGrain 3 >>= withMultipliable
        "tri\x1ec7"  -> double 1e6 >>= withGrain 6 >>= withMultipliable
        "tri\x1ec7u" -> double 1e6 >>= withGrain 6 >>= withMultipliable
        "t"          -> double 1e9 >>= withGrain 9 >>= withMultipliable
        "t\x1ef7"    -> double 1e9 >>= withGrain 9 >>= withMultipliable
        _            -> Nothing
      _ -> Nothing
  }

ruleIntersectWithAnd :: Rule
ruleIntersectWithAnd = Rule
  { name = "intersect (with and)"
  , pattern =
    [ numberWith (fromMaybe 0 . TNumber.grain) (>1)
    , regex "and"
    , numberWith TNumber.multipliable not
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumberData {TNumber.value = val1, TNumber.grain = Just g}):
       _:
       Token Numeral (NumberData {TNumber.value = val2}):
       _) | (10 ** fromIntegral g) > val2 -> double $ val1 + val2
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

ruleNumbersPrefixWithM :: Rule
ruleNumbersPrefixWithM = Rule
  { name = "numbers prefix with -, âm"
  , pattern =
    [ regex "-|\x00e2m\\s?"
    , dimension Numeral
    ]
  , prod = \tokens -> case tokens of
      (_:Token Numeral nd:_) -> double (TNumber.value nd * (-1))
      _ -> Nothing
  }

ruleNumbers2 :: Rule
ruleNumbers2 = Rule
  { name = "numbers 25 35 45 55 65 75 85 95"
  , pattern =
    [ oneOf [20, 30 .. 90]
    , regex "l\x0103m"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumberData {TNumber.value = v}):_) -> double $ v + 5
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

ruleDecimalNumber :: Rule
ruleDecimalNumber = Rule
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
  { name = "integer 21..99"
  , pattern =
    [ oneOf [20, 30 .. 90]
    , numberBetween 1 10
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumberData {TNumber.value = v1}):
       Token Numeral (NumberData {TNumber.value = v2}):
       _) -> double $ v1 + v2
      _ -> Nothing
  }

ruleNumberDot :: Rule
ruleNumberDot = Rule
  { name = "number dot 1 9"
  , pattern =
    [ dimension Numeral
    , regex "ch\x1ea5m|ph\x1ea9y"
    , numberWith TNumber.grain isNothing
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral nd1:_:Token Numeral nd2:_) ->
        double $ TNumber.value nd1 + decimalsToDouble (TNumber.value nd2)
      _ -> Nothing
  }

ruleIntersect :: Rule
ruleIntersect = Rule
  { name = "intersect"
  , pattern =
    [ numberWith (fromMaybe 0 . TNumber.grain) (>1)
    , numberWith TNumber.multipliable not
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumberData {TNumber.value = val1, TNumber.grain = Just g}):
       Token Numeral (NumberData {TNumber.value = val2}):
       _) | (10 ** fromIntegral g) > val2 -> double $ val1 + val2
      _ -> Nothing
  }

ruleMultiply :: Rule
ruleMultiply = Rule
  { name = "compose by multiplication"
  , pattern =
    [ dimension Numeral
    , numberWith TNumber.multipliable id
    ]
  , prod = \tokens -> case tokens of
      (token1:token2:_) -> multiply token1 token2
      _ -> Nothing
  }

ruleNumbersSuffixesKMG :: Rule
ruleNumbersSuffixesKMG = Rule
  { name = "numbers suffixes (K, M, G)"
  , pattern =
    [ dimension Numeral
    , regex "([kmg])(?=[\\W\\$\x20ac]|$)"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumberData {TNumber.value = v}):
       Token RegexMatch (GroupMatch (match:_)):
       _) -> case Text.toLower match of
         "k" -> double $ v * 1e3
         "m" -> double $ v * 1e6
         "g" -> double $ v * 1e9
         _   -> Nothing
      _ -> Nothing
  }

ruleNumberNghn :: Rule
ruleNumberNghn = Rule
  { name = "number nghìn"
  , pattern =
    [ numberBetween 1 1000
    , numberWith TNumber.value (== 1000)
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumberData {TNumber.value = v1}):Token Numeral (NumberData {TNumber.value = v2, TNumber.grain = Just g}):_) ->
        double (v1 * v2) >>= withGrain g
      _ -> Nothing
  }

ruleInteger :: Rule
ruleInteger = Rule
  { name = "integer (0..19)"
  , pattern =
    [ regex "(kh\x00f4ng|m\x1ed9t|linh m\x1ed9t|l\x1ebb m\x1ed9t|hai|linh hai|l\x1ebb hai|ba|linh ba|l\x1ebb ba|b\x1ed1n|linh b\x1ed1n|l\x1ebb b\x1ed1n|n\x0103m|linh n\x0103m|l\x1ebb n\x0103m|s\x00e1u|l\x1ebb s\x00e1u|linh s\x00e1u|b\x1ea3y|l\x1ebb b\x1ea3y|linh b\x1ea3y|t\x00e1m|linh t\x00e1m|l\x1ebb t\x00e1m|ch\x00edn|linh ch\x00edn|l\x1ebb ch\x00edn|m\x01b0\x1eddi m\x1ed9t|m\x01b0\x1eddi hai|m\x01b0\x1eddi ba|m\x01b0\x1eddi b\x1ed1n|m\x01b0\x1eddi l\x0103m|m\x01b0\x1eddi s\x00e1u|m\x01b0\x1eddi b\x1ea3y|m\x01b0\x1eddi t\x00e1m|m\x01b0\x1eddi ch\x00edn|m\x01b0\x1eddi|linh m\x01b0\x1eddi)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case match of
        "kh\x00f4ng" -> integer 0
        "m\x1ed9t" -> integer 1
        "linh m\x1ed9t" -> integer 1
        "l\x1ebb m\x1ed9t" -> integer 1
        "hai" -> integer 2
        "l\x1ebb hai" -> integer 2
        "linh hai" -> integer 2
        "ba" -> integer 3
        "l\x1ebb" -> integer 3
        "linh ba" -> integer 3
        "l\x1ebb b\x1ed1n" -> integer 4
        "linh b\x1ed1n" -> integer 4
        "b\x1ed1n" -> integer 4
        "n\x0103m" -> integer 5
        "l\x1ebb n\x0103m" -> integer 5
        "linh n\x0103m" -> integer 5
        "linh s\x00e1u" -> integer 6
        "s\x00e1u" -> integer 6
        "l\x1ebb s\x00e1u" -> integer 6
        "linh b\x1ea3y" -> integer 7
        "l\x1ebb b\x1ea3y" -> integer 7
        "b\x1ea3y" -> integer 7
        "l\x1ebb t\x00e1m" -> integer 8
        "linh t\x00e1m" -> integer 8
        "t\x00e1m" -> integer 8
        "l\x1ebb ch\x00edn" -> integer 9
        "ch\x00edn" -> integer 9
        "linh ch\x00edn" -> integer 9
        "linh m\x01b0\x1eddi" -> integer 10
        "m\x01b0\x1eddi" -> integer 10
        "l\x1ebb m\x01b0\x1eddi" -> integer 10
        "m\x01b0\x1eddi m\x1ed9t" -> integer 11
        "m\x01b0\x1eddi hai" -> integer 12
        "m\x01b0\x1eddi ba" -> integer 13
        "m\x01b0\x1eddi b\x1ed1n" -> integer 14
        "m\x01b0\x1eddi l\x0103m" -> integer 15
        "m\x01b0\x1eddi s\x00e1u" -> integer 16
        "m\x01b0\x1eddi b\x1ea3y" -> integer 17
        "m\x01b0\x1eddi t\x00e1m" -> integer 18
        "m\x01b0\x1eddi ch\x00edn" -> integer 19
        _ -> Nothing
      _ -> Nothing
  }

ruleInteger2 :: Rule
ruleInteger2 = Rule
  { name = "integer (20..90)"
  , pattern =
    [ regex "(hai m\x01b0\x01a1i|ba m\x01b0\x01a1i|b\x1ed1n m\x01b0\x01a1i|n\x0103m m\x01b0\x01a1i|s\x00e1u m\x01b0\x01a1i|b\x1ea3y m\x01b0\x01a1i|t\x00e1m m\x01b0\x01a1i|ch\x00edn m\x01b0\x01a1i)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case match of
        "hai m\x01b0\x01a1i" -> integer 20
        "ba m\x01b0\x01a1i" -> integer 30
        "b\x1ed1n m\x01b0\x01a1i" -> integer 40
        "n\x0103m m\x01b0\x01a1i" -> integer 50
        "s\x00e1u m\x01b0\x01a1i" -> integer 60
        "b\x1ea3y m\x01b0\x01a1i" -> integer 70
        "t\x00e1m m\x01b0\x01a1i" -> integer 80
        "ch\x00edn m\x01b0\x01a1i" -> integer 90
        _ -> Nothing
      _ -> Nothing
  }

ruleNumbers :: Rule
ruleNumbers = Rule
  { name = "numbers 21 31 41 51 61 71 81 91"
  , pattern =
    [ oneOf [20, 30 .. 90]
    , regex "m\x1ed1t"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumberData {TNumber.value = v}):_) -> double $ v + 1
      _ -> Nothing
  }

ruleT :: Rule
ruleT = Rule
  { name = "tá"
  , pattern =
    [ regex "t\x00e1"
    ]
  , prod = \_ -> integer 12 >>= withGrain 1 >>= withMultipliable
  }

ruleIntegerWithThousandsSeparator :: Rule
ruleIntegerWithThousandsSeparator = Rule
  { name = "integer with thousands separator ,"
  , pattern =
    [ regex "(\\d{1,3}(,\\d\\d\\d){1,5})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):
       _) -> let fmt = Text.replace (Text.singleton ',') Text.empty match
        in parseDouble fmt >>= double
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleDecimalNumber
  , ruleDecimalWithThousandsSeparator
  , ruleInteger
  , ruleInteger2
  , ruleInteger3
  , ruleIntegerNumeric
  , ruleIntegerWithThousandsSeparator
  , ruleIntersect
  , ruleIntersectWithAnd
  , ruleMultiply
  , ruleNumberDot
  , ruleNumberNghn
  , ruleNumbers
  , ruleNumbers2
  , ruleNumbersPrefixWithM
  , ruleNumbersSuffixesKMG
  , rulePowersOfTen
  , ruleT
  ]
