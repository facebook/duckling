-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.DE.Rules
  ( rules
  ) where

import Data.HashMap.Strict (HashMap)
import Data.Maybe
import Data.Text (Text)
import Prelude
import Data.String
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Regex.Types
import Duckling.Types
import qualified Duckling.Numeral.Types as TNumeral
import Duckling.Numeral.DE.NumParser

ruleNumeralsPrefixWithNegativeOrMinus :: Rule
ruleNumeralsPrefixWithNegativeOrMinus = Rule
  { name = "numbers prefix with -, negative or minus"
  , pattern =
    [ regex "-|minus|negativ"
    , Predicate isPositive
    ]
  , prod = \tokens -> case tokens of
      (_:Token Numeral NumeralData{TNumeral.value = v}:_) ->
        double $ v * (- 1)
      _ -> Nothing
  }

ruleFew :: Rule
ruleFew = Rule
  { name = "few"
  , pattern =
    [ regex "mehrere"
    ]
  , prod = \_ -> integer 3
  }

ruleDecimalWithSwissThousandsSeparator :: Rule
ruleDecimalWithSwissThousandsSeparator = Rule
  { name = "decimal with thousands separator"
  , pattern =
    [ regex "(\\d+(\\'\\d\\d\\d)+\\,\\d+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):
       _) -> let fmt = Text.replace "," "." $ Text.replace "." Text.empty match
        in parseDouble fmt >>= double
      _ -> Nothing
  }

ruleDecimalWithThousandsSeparator :: Rule
ruleDecimalWithThousandsSeparator = Rule
  { name = "decimal with thousands separator"
  , pattern =
    [ regex "(\\d+(\\.\\d\\d\\d)+\\,\\d+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):
       _) -> let fmt = Text.replace "," "." $ Text.replace "." Text.empty match
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
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        parseDecimal False match
      _ -> Nothing
  }

ruleNumeralsUnd :: Rule
ruleNumeralsUnd = Rule
  { name = "numbers und"
  , pattern =
    [ Predicate $ numberBetween 1 10
    , regex "und"
    , oneOf [20, 30 .. 90]
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v1}:
       _:
       Token Numeral NumeralData{TNumeral.value = v2}:
       _) -> double $ v1 + v2
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
         _ -> Nothing
      _ -> Nothing
  }

ruleCouple :: Rule
ruleCouple = Rule
  { name = "couple"
  , pattern =
    [ regex "(ein )?paar"
    ]
  , prod = \_ -> integer 2
  }

ruleDozen :: Rule
ruleDozen = Rule
  { name = "dozen"
  , pattern =
    [ regex "dutzend"
    ]
  , prod = \_ -> integer 12 >>= withGrain 1 >>= withMultipliable
  }

rulePowersOfTen :: Rule
rulePowersOfTen = Rule
  { name = "powers of tens"
  , pattern =
    [ regex "(hunderte?|tausende?|million(en)?|milliarde(n)?|billion(en)?|billiarde(n)?)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "hundert"    -> double 1e2  >>= withGrain 2  >>= withMultipliable
        "hunderte"   -> double 1e2  >>= withGrain 2  >>= withMultipliable
        "tausend"    -> double 1e3  >>= withGrain 3  >>= withMultipliable
        "tausende"   -> double 1e3  >>= withGrain 3  >>= withMultipliable
        "million"    -> double 1e6  >>= withGrain 6  >>= withMultipliable
        "millionen"  -> double 1e6  >>= withGrain 6  >>= withMultipliable
        "milliarde"  -> double 1e9  >>= withGrain 9  >>= withMultipliable
        "milliarden" -> double 1e9  >>= withGrain 9  >>= withMultipliable
        "billion"    -> double 1e12 >>= withGrain 12 >>= withMultipliable
        "billionen"  -> double 1e12 >>= withGrain 12 >>= withMultipliable
        "billiarde"  -> double 1e15 >>= withGrain 15 >>= withMultipliable
        "billiarden" -> double 1e15 >>= withGrain 15 >>= withMultipliable
        _            -> Nothing
      _ -> Nothing
  }

ruleZero :: Rule
ruleZero = Rule
  { name = "integer 0"
  , pattern =
    [ regex "(keine(m|n|r|s)?|keins?|null|nichts)"
    ]
  , prod = \_ -> integer 0
  }

ruleNumeralDotNumeral :: Rule
ruleNumeralDotNumeral = Rule
  { name = "number dot number"
  , pattern =
    [ dimension Numeral
    , regex "komma"
    , Predicate $ not . hasGrain
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v1}:
       _:
       Token Numeral NumeralData{TNumeral.value = v2}:
       _) -> double $ v1 + decimalsToDouble v2
      _ -> Nothing
  }

ruleIntegerWithSwissThousandsSeparator :: Rule
ruleIntegerWithSwissThousandsSeparator = Rule
  { name = "integer with thousands separator ."
  , pattern =
    [ regex "(\\d{1,3}(\\'\\d\\d\\d){1,5})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        parseDouble (Text.replace "'" Text.empty match) >>= double
      _ -> Nothing
  }

ruleIntegerWithThousandsSeparator :: Rule
ruleIntegerWithThousandsSeparator = Rule
  { name = "integer with thousands separator ."
  , pattern =
    [ regex "(\\d{1,3}(\\.\\d\\d\\d){1,5})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        parseDouble (Text.replace "." Text.empty match) >>= double
      _ -> Nothing
  }

ruleAllNumeralWords :: Rule
ruleAllNumeralWords = Rule
  { name = "simple and complex numerals written as one word"
  , pattern = [regex "(ein|zwei|drei|vier|fünf|sech|sieb|acht|neun|zehn|elf|zwölf|hundert|tausend)?([^\\s]+)?(eine[m|n|r|s]?|eins?|zwei|drei|vier|fünf|sechs|sieben|acht|neun|zehn|elf|zwölf|[s|ß|z]ig|hundert|tausend)"]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch matches) : _) ->
        (parseNumeral $ concat $ Text.unpack . Text.toLower <$> matches)
        >>= integer
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleCouple
  , ruleDecimalNumeral
  , ruleDecimalWithThousandsSeparator
  , ruleDecimalWithSwissThousandsSeparator
  , ruleDozen
  , ruleFew
  , ruleIntegerWithThousandsSeparator
  , ruleIntegerWithSwissThousandsSeparator
  , ruleIntersect
  , ruleMultiply
  , ruleNumeralDotNumeral
  , ruleNumeralsPrefixWithNegativeOrMinus
  , ruleNumeralsSuffixesKMG
  , ruleNumeralsUnd
  , rulePowersOfTen
  , ruleZero
  , ruleAllNumeralWords
  ]