-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.VI.Rules
  ( rules ) where

import qualified Data.HashMap.Strict as HashMap
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

powersOfTenMap :: HashMap.HashMap Text.Text (Double, Int)
powersOfTenMap = HashMap.fromList
  [ ( "tr\x0103",   (1e2, 2) )
  , ( "tr\x0103m",  (1e2, 2) )
  , ( "ngh\x00ec",  (1e3, 3) )
  , ( "ngh\x00ecn", (1e3, 3) )
  , ( "tri\x1ec7",  (1e6, 6) )
  , ( "tri\x1ec7u", (1e6, 6) )
  , ( "t",          (1e9, 9) )
  , ( "t\x1ef7",    (1e9, 9) )
  ]

rulePowersOfTen :: Rule
rulePowersOfTen = Rule
  { name = "powers of tens"
  , pattern =
    [ regex "(tr\x0103m?|ngh\x00ecn?|tri\x1ec7u?|t\x1ef7?)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        do
          (value, grain) <- HashMap.lookup (Text.toLower match) powersOfTenMap
          double value >>= withGrain grain >>= withMultipliable
      _ -> Nothing
  }

ruleIntersectWithAnd :: Rule
ruleIntersectWithAnd = Rule
  { name = "intersect (with and)"
  , pattern =
    [ numberWith (fromMaybe 0 . TNumeral.grain) (>1)
    , regex "and"
    , numberWith TNumeral.multipliable not
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = val1, TNumeral.grain = Just g}):
       _:
       Token Numeral (NumeralData {TNumeral.value = val2}):
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

ruleNumeralsPrefixWithM :: Rule
ruleNumeralsPrefixWithM = Rule
  { name = "numbers prefix with -, âm"
  , pattern =
    [ regex "-|\x00e2m\\s?"
    , dimension Numeral
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
    , regex "l\x0103m"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = v}):_) -> double $ v + 5
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

ruleInteger3 :: Rule
ruleInteger3 = Rule
  { name = "integer 21..99"
  , pattern =
    [ oneOf [20, 30 .. 90]
    , numberBetween 1 10
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = v1}):
       Token Numeral (NumeralData {TNumeral.value = v2}):
       _) -> double $ v1 + v2
      _ -> Nothing
  }

ruleNumeralDot :: Rule
ruleNumeralDot = Rule
  { name = "number dot 1 9"
  , pattern =
    [ dimension Numeral
    , regex "ch\x1ea5m|ph\x1ea9y"
    , numberWith TNumeral.grain isNothing
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
    [ numberWith (fromMaybe 0 . TNumeral.grain) (>1)
    , numberWith TNumeral.multipliable not
    ]
  , prod = \tokens -> case tokens of
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

ruleNumeralsSuffixesKMG :: Rule
ruleNumeralsSuffixesKMG = Rule
  { name = "numbers suffixes (K, M, G)"
  , pattern =
    [ dimension Numeral
    , regex "([kmg])(?=[\\W\\$\x20ac]|$)"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = v}):
       Token RegexMatch (GroupMatch (match:_)):
       _) -> case Text.toLower match of
         "k" -> double $ v * 1e3
         "m" -> double $ v * 1e6
         "g" -> double $ v * 1e9
         _   -> Nothing
      _ -> Nothing
  }

ruleNumeralNghn :: Rule
ruleNumeralNghn = Rule
  { name = "number nghìn"
  , pattern =
    [ numberBetween 1 1000
    , numberWith TNumeral.value (== 1000)
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = v1}):
       Token Numeral (NumeralData {TNumeral.value = v2, TNumeral.grain = Just g}):
       _) -> double (v1 * v2) >>= withGrain g
      _ -> Nothing
  }

integerMap :: HashMap.HashMap Text.Text Integer
integerMap = HashMap.fromList
  [ ("kh\x00f4ng",               0)
  , ("m\x1ed9t",                 1)
  , ("linh m\x1ed9t",            1)
  , ("l\x1ebb m\x1ed9t",         1)
  , ("hai",                      2)
  , ("l\x1ebb hai",              2)
  , ("linh hai",                 2)
  , ("ba",                       3)
  , ("l\x1ebb",                  3)
  , ("linh ba",                  3)
  , ("l\x1ebb b\x1ed1n",         4)
  , ("linh b\x1ed1n",            4)
  , ("b\x1ed1n",                 4)
  , ("n\x0103m",                 5)
  , ("l\x1ebb n\x0103m",         5)
  , ("linh n\x0103m",            5)
  , ("linh s\x00e1u",            6)
  , ("s\x00e1u",                 6)
  , ("l\x1ebb s\x00e1u",         6)
  , ("linh b\x1ea3y",            7)
  , ("l\x1ebb b\x1ea3y",         7)
  , ("b\x1ea3y",                 7)
  , ("l\x1ebb t\x00e1m",         8)
  , ("linh t\x00e1m",            8)
  , ("t\x00e1m",                 8)
  , ("l\x1ebb ch\x00edn",        9)
  , ("ch\x00edn",                9)
  , ("linh ch\x00edn",           9)
  , ("linh m\x01b0\x1eddi",      10)
  , ("m\x01b0\x1eddi",           10)
  , ("l\x1ebb m\x01b0\x1eddi",   10)
  , ("m\x01b0\x1eddi m\x1ed9t",  11)
  , ("m\x01b0\x1eddi hai",       12)
  , ("m\x01b0\x1eddi ba",        13)
  , ("m\x01b0\x1eddi b\x1ed1n",  14)
  , ("m\x01b0\x1eddi l\x0103m",  15)
  , ("m\x01b0\x1eddi s\x00e1u",  16)
  , ("m\x01b0\x1eddi b\x1ea3y",  17)
  , ("m\x01b0\x1eddi t\x00e1m",  18)
  , ("m\x01b0\x1eddi ch\x00edn", 19)
  ]

ruleInteger :: Rule
ruleInteger = Rule
  { name = "integer (0..19)"
  , pattern =
    [ regex "(kh\x00f4ng|m\x1ed9t|linh m\x1ed9t|l\x1ebb m\x1ed9t|hai|linh hai|l\x1ebb hai|ba|linh ba|l\x1ebb ba|b\x1ed1n|linh b\x1ed1n|l\x1ebb b\x1ed1n|n\x0103m|linh n\x0103m|l\x1ebb n\x0103m|s\x00e1u|l\x1ebb s\x00e1u|linh s\x00e1u|b\x1ea3y|l\x1ebb b\x1ea3y|linh b\x1ea3y|t\x00e1m|linh t\x00e1m|l\x1ebb t\x00e1m|ch\x00edn|linh ch\x00edn|l\x1ebb ch\x00edn|m\x01b0\x1eddi m\x1ed9t|m\x01b0\x1eddi hai|m\x01b0\x1eddi ba|m\x01b0\x1eddi b\x1ed1n|m\x01b0\x1eddi l\x0103m|m\x01b0\x1eddi s\x00e1u|m\x01b0\x1eddi b\x1ea3y|m\x01b0\x1eddi t\x00e1m|m\x01b0\x1eddi ch\x00edn|m\x01b0\x1eddi|linh m\x01b0\x1eddi)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) integerMap >>= integer
      _ -> Nothing
  }

tensMap :: HashMap.HashMap Text.Text Integer
tensMap = HashMap.fromList
  [ ("hai m\x01b0\x01a1i",       20)
  , ("ba m\x01b0\x01a1i",        30)
  , ("b\x1ed1n m\x01b0\x01a1i",  40)
  , ("n\x0103m m\x01b0\x01a1i",  50)
  , ("s\x00e1u m\x01b0\x01a1i",  60)
  , ("b\x1ea3y m\x01b0\x01a1i",  70)
  , ("t\x00e1m m\x01b0\x01a1i",  80)
  , ("ch\x00edn m\x01b0\x01a1i", 90)
  ]

ruleInteger2 :: Rule
ruleInteger2 = Rule
  { name = "integer (20..90)"
  , pattern =
    [ regex "(hai m\x01b0\x01a1i|ba m\x01b0\x01a1i|b\x1ed1n m\x01b0\x01a1i|n\x0103m m\x01b0\x01a1i|s\x00e1u m\x01b0\x01a1i|b\x1ea3y m\x01b0\x01a1i|t\x00e1m m\x01b0\x01a1i|ch\x00edn m\x01b0\x01a1i)"
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
    , regex "m\x1ed1t"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = v}):_) -> double $ v + 1
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
  [ ruleDecimalNumeral
  , ruleDecimalWithThousandsSeparator
  , ruleInteger
  , ruleInteger2
  , ruleInteger3
  , ruleIntegerNumeric
  , ruleIntegerWithThousandsSeparator
  , ruleIntersect
  , ruleIntersectWithAnd
  , ruleMultiply
  , ruleNumeralDot
  , ruleNumeralNghn
  , ruleNumerals
  , ruleNumerals2
  , ruleNumeralsPrefixWithM
  , ruleNumeralsSuffixesKMG
  , rulePowersOfTen
  , ruleT
  ]
