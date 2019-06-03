-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.FR.Rules
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

ruleNumerals4 :: Rule
ruleNumerals4 = Rule
  { name = "numbers 81"
  , pattern =
    [ numberWith TNumeral.value (== 80)
    , numberWith TNumeral.value (== 1)
    ]
  , prod = \_ -> integer 81
  }

ruleNumeralsPrefixWithNegativeOrMinus :: Rule
ruleNumeralsPrefixWithNegativeOrMinus = Rule
  { name = "numbers prefix with -, negative or minus"
  , pattern =
    [ regex "-|moins"
    , dimension Numeral
    ]
  , prod = \tokens -> case tokens of
      (_:
       Token Numeral NumeralData{TNumeral.value = v}:
       _) -> double $ v * (-1)
      _ -> Nothing
  }

ruleNumerals2 :: Rule
ruleNumerals2 = Rule
  { name = "numbers 22..29 32..39 .. 52..59"
  , pattern =
    [ oneOf [20, 50, 40, 30]
    , regex "[\\s\\-]+"
    , numberBetween 2 10
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v1}:
       _:
       Token Numeral NumeralData{TNumeral.value = v2}:
       _) -> double $ v1 + v2
      _ -> Nothing
  }

ruleDecimalWithThousandsSeparator :: Rule
ruleDecimalWithThousandsSeparator = Rule
  { name = "decimal with thousands separator"
  , pattern =
    [ regex "(\\d+(([\\. ])\\d\\d\\d)+,\\d+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_:sep:_)):
       _) -> let fmt = Text.replace "," "." $ Text.replace sep Text.empty match
        in parseDouble fmt >>= double
      _ -> Nothing
  }

ruleDecimalNumeral :: Rule
ruleDecimalNumeral = Rule
  { name = "decimal number"
  , pattern =
    [ regex "(\\d*,\\d+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):
       _) -> parseDecimal False match
      _ -> Nothing
  }

ruleNumeral2Map :: HashMap Text Integer
ruleNumeral2Map = HashMap.fromList
  [ ( "vingt"    , 20 )
  , ( "trente"   , 30 )
  , ( "quarante" , 40 )
  , ( "cinquante", 50 )
  , ( "soixante" , 60 )
  ]

ruleNumeral2 :: Rule
ruleNumeral2 = Rule
  { name = "number (20..60)"
  , pattern =
    [ regex "(vingt|trente|quarante|cinquante|soixante)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) ruleNumeral2Map >>= integer
      _ -> Nothing
  }

ruleNumerals5 :: Rule
ruleNumerals5 = Rule
  { name = "numbers 62..69 .. 92..99"
  , pattern =
    [ oneOf [60, 80]
    , regex "[\\s\\-]+"
    , numberBetween 2 20
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v1}:
       _:
       Token Numeral NumeralData{TNumeral.value = v2}:
       _) -> double $ v1 + v2
      _ -> Nothing
  }

ruleNumeralMap :: HashMap Text Integer
ruleNumeralMap = HashMap.fromList
  [ ( "zero"     , 0 )
  , ( "zéro"     , 0 )
  , ( "un"       , 1 )
  , ( "une"      , 1 )
  , ( "deux"     , 2 )
  , ( "trois"    , 3 )
  , ( "quatre"   , 4 )
  , ( "cinq"     , 5 )
  , ( "six"      , 6 )
  , ( "sept"     , 7 )
  , ( "huit"     , 8 )
  , ( "neuf"     , 9 )
  , ( "dix"      , 10 )
  , ( "onze"     , 11 )
  , ( "douze"    , 12 )
  , ( "treize"   , 13 )
  , ( "quatorze" , 14 )
  , ( "quinze"   , 15 )
  , ( "seize"    , 16 )
  ]

ruleNumeral :: Rule
ruleNumeral = Rule
  { name = "number (0..16)"
  , pattern =
    [ regex "(z(e|é)ro|une?|deux|trois|quatre|cinq|six|sept|huit|neuf|dix|onze|douze|treize|quatorze|quinze|seize)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) ruleNumeralMap >>= integer
      _ -> Nothing
  }

ruleNumeral3 :: Rule
ruleNumeral3 = Rule
  { name = "number (17..19)"
  , pattern =
    [ numberWith TNumeral.value (== 10)
    , regex "[\\s\\-]+"
    , numberBetween 7 10
    ]
  , prod = \tokens -> case tokens of
      (_:
       _:
       Token Numeral NumeralData{TNumeral.value = v}:
       _) -> double $ 10 + v
      _ -> Nothing
  }

ruleNumerals3 :: Rule
ruleNumerals3 = Rule
  { name = "numbers 61 71"
  , pattern =
    [ numberWith TNumeral.value (== 60)
    , regex "-?et-?"
    , oneOf [1, 11]
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v1}:
       _:
       Token Numeral NumeralData{TNumeral.value = v2}:
       _) -> double $ v1 + v2
      _ -> Nothing
  }

ruleNumeralsSuffixesKMG :: Rule
ruleNumeralsSuffixesKMG = Rule
  { name = "numbers suffixes (K, M, G)"
  , pattern =
    [ dimension Numeral
    , regex "([kmg])(?=[\\W$€¢£]|$)"
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

ruleNumeral4 :: Rule
ruleNumeral4 = Rule
  { name = "number 80"
  , pattern =
    [ regex "quatre"
    , regex "vingts?"
    ]
  , prod = \_ -> integer 80
  }

ruleNumerals :: Rule
ruleNumerals = Rule
  { name = "numbers 21 31 41 51"
  , pattern =
    [ oneOf [20, 50, 40, 30]
    , regex "-?et-?"
    , numberWith TNumeral.value (== 1)
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v1}:
       _:
       Token Numeral NumeralData{TNumeral.value = v2}:
       _) -> double $ v1 + v2
      _ -> Nothing
  }

ruleIntegerWithThousandsSeparator :: Rule
ruleIntegerWithThousandsSeparator = Rule
  { name = "integer with thousands separator ."
  , pattern =
    [ regex "(\\d{1,3}(([\\. ])\\d\\d\\d){1,5})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_:sep:_)):
       _) -> let fmt = Text.replace sep Text.empty match
        in parseDouble fmt >>= double
      _ -> Nothing
  }

rulePowersOfTen :: Rule
rulePowersOfTen = Rule
  { name = "powers of tens"
  , pattern =
    [ regex "(cent|mille|millions?|milliards?)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "cent"      -> double 1e2 >>= withGrain 2 >>= withMultipliable
        "mille"     -> double 1e3 >>= withGrain 3 >>= withMultipliable
        "million"   -> double 1e6 >>= withGrain 6 >>= withMultipliable
        "millions"  -> double 1e6 >>= withGrain 6 >>= withMultipliable
        "milliard"  -> double 1e9 >>= withGrain 9 >>= withMultipliable
        "milliards" -> double 1e9 >>= withGrain 9 >>= withMultipliable
        _           -> Nothing
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

rules :: [Rule]
rules =
  [ ruleDecimalNumeral
  , ruleDecimalWithThousandsSeparator
  , ruleIntegerWithThousandsSeparator
  , ruleNumeral
  , ruleNumeral2
  , ruleNumeral3
  , ruleNumeral4
  , ruleNumerals
  , ruleNumerals2
  , ruleNumerals3
  , ruleNumerals4
  , ruleNumerals5
  , ruleNumeralsPrefixWithNegativeOrMinus
  , ruleNumeralsSuffixesKMG
  , rulePowersOfTen
  , ruleSum
  , ruleMultiply
  ]
