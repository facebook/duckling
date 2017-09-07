-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.ET.Rules
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

ruleIntegerWithThousandsSeparatorSpace :: Rule
ruleIntegerWithThousandsSeparatorSpace = Rule
  { name = "integer with thousands separator space"
  , pattern =
    [ regex "(\\d{1,3}(\\s\\d\\d\\d){1,5})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        parseDouble (Text.replace (Text.singleton ' ') Text.empty match) >>= double
      _ -> Nothing
  }

ruleNumeralsPrefixWithNegativeOrMinus :: Rule
ruleNumeralsPrefixWithNegativeOrMinus = Rule
  { name = "numbers prefix with -, negative or minus"
  , pattern =
    [ regex "-|miinus|negatiivne"
    , dimension Numeral
    ]
  , prod = \tokens -> case tokens of
      (_:
       Token Numeral (NumeralData {TNumeral.value = v}):
       _) -> double $ v * (-1)
      _ -> Nothing
  }

ruleIntegerNumeric :: Rule
ruleIntegerNumeric = Rule
  { name = "integer (numeric)"
  , pattern =
    [ regex "(\\d{1,18})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):
       _) -> do
         v <- parseInt match
         integer $ toInteger v
      _ -> Nothing
  }

ruleACoupleOf :: Rule
ruleACoupleOf = Rule
  { name = "a couple of"
  , pattern =
    [ regex "paar"
    ]
  , prod = \_ -> integer 2
  }

ruleTen :: Rule
ruleTen = Rule
  { name = "ten"
  , pattern =
    [ regex "kümme"
    ]
  , prod = \_ -> integer 10 >>= withGrain 1
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
    [ oneOf [70, 20, 60, 50, 40, 90, 30, 80]
    , numberBetween 1 10
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = v1}):
       Token Numeral (NumeralData {TNumeral.value = v2}):_) -> double $ v1 + v2
      _ -> Nothing
  }

ruleAFew :: Rule
ruleAFew = Rule
  { name = "(a )?few"
  , pattern =
    [ regex "mõni"
    ]
  , prod = \_ -> integer 3
  }

rulePowersOfTen :: Rule
rulePowersOfTen = Rule
  { name = "powers of tens"
  , pattern = [regex "(sada|tuhat|miljoni?t?)"]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "sada"  -> double 1e2 >>= withGrain 2 >>= withMultipliable
        "tuhat" -> double 1e3 >>= withGrain 3 >>= withMultipliable
        _       -> double 1e6 >>= withGrain 6 >>= withMultipliable
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

ruleNumeralsSuffixesKMG :: Rule
ruleNumeralsSuffixesKMG = Rule
  { name = "numbers suffixes (K, M, G)"
  , pattern =
    [ dimension Numeral
    , regex "([kmg])(?=[\\W\\$€]|$)"
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

ruleInteger :: Rule
ruleInteger = Rule
  { name = "integer (0..19)"
  , pattern =
    [ regex "(null|üksteist|üks|kaksteist|kaks|kolmteist|kolm|neliteist|neli|viisteist|viis|kuusteist|kuus|seitseteist|seitse|kaheksateist|kaheksa|üheksateist|üheksa|kümme)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "null" -> integer 0
        "üks" -> integer 1
        "kaks" -> integer 2
        "kolm" -> integer 3
        "neli" -> integer 4
        "viis" -> integer 5
        "kuus" -> integer 6
        "seitse" -> integer 7
        "kaheksa" -> integer 8
        "üheksa" -> integer 9
        "kümme" -> integer 10
        "üksteist" -> integer 11
        "kaksteist" -> integer 12
        "kolmteist" -> integer 13
        "neliteist" -> integer 14
        "viisteist" -> integer 15
        "kuusteist" -> integer 16
        "seitseteist" -> integer 17
        "kaheksateist" -> integer 18
        "üheksateist" -> integer 19
        _ -> Nothing
      _ -> Nothing
  }

ruleInteger4 :: Rule
ruleInteger4 = Rule
  { name = "integer (200..900)"
  , pattern =
    [ regex "(kakssada|kolmsada|nelisada|viissada|kuussada|seitsesada|kaheksasada|üheksasada)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "kakssada"        -> integer 200 >>= withGrain 2 >>= withMultipliable
        "kolmsada"        -> integer 300 >>= withGrain 2 >>= withMultipliable
        "nelisada"        -> integer 400 >>= withGrain 2 >>= withMultipliable
        "viissada"        -> integer 500 >>= withGrain 2 >>= withMultipliable
        "kuussada"        -> integer 600 >>= withGrain 2 >>= withMultipliable
        "seitsesada"      -> integer 700 >>= withGrain 2 >>= withMultipliable
        "kaheksasada"     -> integer 800 >>= withGrain 2 >>= withMultipliable
        "üheksasada" -> integer 900 >>= withGrain 2 >>= withMultipliable
        _ -> Nothing
      _ -> Nothing
  }

ruleInteger2 :: Rule
ruleInteger2 = Rule
  { name = "integer (20..90)"
  , pattern =
    [ regex "((kaks|kolm|neli|viis|kuus|seitse|kaheksa|(ü)heksa)k(ü)mmend)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "kakskümmend" -> integer 20
        "kolmkümmend" -> integer 30
        "nelikümmend" -> integer 40
        "viiskümmend" -> integer 50
        "kuuskümmend" -> integer 60
        "seitsekümmend" -> integer 70
        "kaheksakümmend" -> integer 80
        "üheksakümmend" -> integer 90
        _ -> Nothing
      _ -> Nothing
  }

ruleNumeralDotNumeral :: Rule
ruleNumeralDotNumeral = Rule
  { name = "number dot number"
  , pattern =
    [ dimension Numeral
    , regex "dot|point"
    , numberWith TNumeral.grain isNothing
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = v1}):
       _:
       Token Numeral (NumeralData {TNumeral.value = v2}):
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
        parseDouble (Text.replace (Text.singleton ',') Text.empty match) >>= double
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleACoupleOf
  , ruleAFew
  , ruleDecimalNumeral
  , ruleDecimalWithThousandsSeparator
  , ruleInteger
  , ruleInteger2
  , ruleInteger3
  , ruleInteger4
  , ruleIntegerNumeric
  , ruleIntegerWithThousandsSeparator
  , ruleIntegerWithThousandsSeparatorSpace
  , ruleIntersect
  , ruleMultiply
  , ruleNumeralDotNumeral
  , ruleNumeralsPrefixWithNegativeOrMinus
  , ruleNumeralsSuffixesKMG
  , rulePowersOfTen
  , ruleTen
  ]
