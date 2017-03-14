-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Number.SV.Rules
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

ruleIntersectWithAnd :: Rule
ruleIntersectWithAnd = Rule
  { name = "intersect (with and)"
  , pattern =
    [ numberWith (fromMaybe 0 . TNumber.grain) (>1)
    , regex "och"
    , numberWith TNumber.multipliable not
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumberData {TNumber.value = val1, TNumber.grain = Just g}):
       _:
       Token Numeral (NumberData {TNumber.value = val2}):
       _) | (10 ** fromIntegral g) > val2 -> double $ val1 + val2
      _ -> Nothing
  }

ruleNumbersPrefixWithNegativeOrMinus :: Rule
ruleNumbersPrefixWithNegativeOrMinus = Rule
  { name = "numbers prefix with -, negative or minus"
  , pattern =
    [ regex "-|minus\\s?|negativ\\s?"
    , dimension Numeral
    ]
  , prod = \tokens -> case tokens of
      (_:
       Token Numeral (NumberData {TNumber.value = v}):
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

ruleFew :: Rule
ruleFew = Rule
  { name = "few"
  , pattern =
    [ regex "(n\x00e5gra )?f\x00e5"
    ]
  , prod = \_ -> integer 3
  }

ruleDecimalWithThousandsSeparator :: Rule
ruleDecimalWithThousandsSeparator = Rule
  { name = "decimal with thousands separator"
  , pattern =
    [ regex "(\\d+(\\.\\d\\d\\d)+\\,\\d+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):
       _) -> let dot = Text.singleton '.'
                 comma = Text.singleton ','
                 fmt = Text.replace comma dot $ Text.replace dot Text.empty match
        in parseDouble fmt >>= double
      _ -> Nothing
  }

ruleDecimalNumber :: Rule
ruleDecimalNumber = Rule
  { name = "decimal number"
  , pattern =
    [ regex "(\\d*,\\d+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):
       _) -> parseDecimal False match
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
      (Token Numeral (NumberData {TNumber.value = v1}):
       Token Numeral (NumberData {TNumber.value = v2}):
       _) -> double $ v1 + v2
      _ -> Nothing
  }

ruleSingle :: Rule
ruleSingle = Rule
  { name = "single"
  , pattern =
    [ regex "enkel"
    ]
  , prod = \_ -> integer 1 >>= withGrain 1
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

rulePowersOfTen :: Rule
rulePowersOfTen = Rule
  { name = "powers of tens"
  , pattern =
    [ regex "(hundra?|tusen?|miljon(er)?)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "hundr"    -> double 1e2 >>= withGrain 2 >>= withMultipliable
        "hundra"   -> double 1e2 >>= withGrain 2 >>= withMultipliable
        "tuse"     -> double 1e3 >>= withGrain 3 >>= withMultipliable
        "tusen"    -> double 1e3 >>= withGrain 3 >>= withMultipliable
        "miljon"   -> double 1e6 >>= withGrain 6 >>= withMultipliable
        "miljoner" -> double 1e6 >>= withGrain 6 >>= withMultipliable
        _          -> Nothing
      _ -> Nothing
  }

ruleCouple :: Rule
ruleCouple = Rule
  { name = "couple, a pair"
  , pattern =
    [ regex "ett par"
    ]
  , prod = \_ -> integer 2
  }

ruleDozen :: Rule
ruleDozen = Rule
  { name = "dozen"
  , pattern =
    [ regex "dussin"
    ]
  , prod = \_ -> integer 12 >>= withGrain 1 >>= withMultipliable
  }

ruleInteger :: Rule
ruleInteger = Rule
  { name = "integer (0..19)"
  , pattern =
    [ regex "(inget|ingen|noll|en|ett|tv\x00e5|tretton|tre|fyra|femton|fem|sexton|sex|sjutton|sju|\x00e5tta|nio|tio|elva|tolv|fjorton|arton|nitton)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "inget" -> integer 0
        "ingen" -> integer 0
        "noll" -> integer 0
        "en" -> integer 1
        "ett" -> integer 1
        "tv\x00e5" -> integer 2
        "tre" -> integer 3
        "fyra" -> integer 4
        "fem" -> integer 5
        "sex" -> integer 6
        "sju" -> integer 7
        "\x00e5tta" -> integer 8
        "nio" -> integer 9
        "tio" -> integer 10
        "elva" -> integer 11
        "tolv" -> integer 12
        "tretton" -> integer 13
        "fjorton" -> integer 14
        "femton" -> integer 15
        "sexton" -> integer 16
        "sjutton" -> integer 17
        "arton" -> integer 18
        "nitton" -> integer 19
        _ -> Nothing
      _ -> Nothing
  }

ruleInteger2 :: Rule
ruleInteger2 = Rule
  { name = "integer (20..90)"
  , pattern =
    [ regex "(tjugo|trettio|fyrtio|femtio|sextio|sjuttio|\x00e5ttio|nittio)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "tjugo" -> integer 20
        "trettio" -> integer 30
        "fyrtio" -> integer 40
        "femtio" -> integer 50
        "sextio" -> integer 60
        "sjuttio" -> integer 70
        "\x00e5ttio" -> integer 80
        "nittio" -> integer 90
        _ -> Nothing
      _ -> Nothing
  }

ruleNumberDotNumber :: Rule
ruleNumberDotNumber = Rule
  { name = "number dot number"
  , pattern =
    [ dimension Numeral
    , regex "komma"
    , numberWith TNumber.grain isNothing
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral nd1:_:Token Numeral nd2:_) ->
        double $ TNumber.value nd1 + decimalsToDouble (TNumber.value nd2)
      _ -> Nothing
  }

ruleIntegerWithThousandsSeparator :: Rule
ruleIntegerWithThousandsSeparator = Rule
  { name = "integer with thousands separator ."
  , pattern =
    [ regex "(\\d{1,3}(\\.\\d\\d\\d){1,5})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):
       _) -> let fmt = Text.replace (Text.singleton '.') Text.empty match
        in parseDouble fmt >>= double
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleCouple
  , ruleDecimalNumber
  , ruleDecimalWithThousandsSeparator
  , ruleDozen
  , ruleFew
  , ruleInteger
  , ruleInteger2
  , ruleInteger3
  , ruleIntegerNumeric
  , ruleIntegerWithThousandsSeparator
  , ruleIntersect
  , ruleIntersectWithAnd
  , ruleMultiply
  , ruleNumberDotNumber
  , ruleNumbersPrefixWithNegativeOrMinus
  , ruleNumbersSuffixesKMG
  , rulePowersOfTen
  , ruleSingle
  ]
