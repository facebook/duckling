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

ruleTen :: Rule
ruleTen = Rule
  { name = "ten"
  , pattern =
    [ regex "zehn"
    ]
  , prod = \_ -> integer 10 >>= withGrain 1
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

-- TODO: Single-word composition (#110)
ruleInteger3 :: Rule
ruleInteger3 = Rule
  { name = "integer ([2-9][1-9])"
  , pattern =
    [ regex "(ein|zwei|drei|vier|fünf|sechs|sieben|acht|neun)und(zwanzig|dreissig|vierzig|fünfzig|sechzig|siebzig|achtzig|neunzig)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:m2:_)):_) -> do
        v1 <- case Text.toLower m1 of
          "ein" -> Just 1
          "zwei" -> Just 2
          "drei" -> Just 3
          "vier" -> Just 4
          "fünf" -> Just 5
          "sechs" -> Just 6
          "sieben" -> Just 7
          "acht" -> Just 8
          "neun" -> Just 9
          _ -> Nothing
        v2 <- case Text.toLower m2 of
          "zwanzig" -> Just 20
          "dreissig" -> Just 30
          "vierzig" -> Just 40
          "fünfzig" -> Just 50
          "sechzig" -> Just 60
          "siebzig" -> Just 70
          "achtzig" -> Just 80
          "neunzig" -> Just 90
          _ -> Nothing
        integer $ v1 + v2
      _ -> Nothing
  }

ruleNumeralsUnd :: Rule
ruleNumeralsUnd = Rule
  { name = "numbers und"
  , pattern =
    [ numberBetween 1 10
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
    [ regex "(hunderte?|tausende?|million(en)?)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "hundert"   -> double 1e2 >>= withGrain 2 >>= withMultipliable
        "hunderte"  -> double 1e2 >>= withGrain 2 >>= withMultipliable
        "tausend"   -> double 1e3 >>= withGrain 3 >>= withMultipliable
        "tausende"  -> double 1e3 >>= withGrain 3 >>= withMultipliable
        "million"   -> double 1e6 >>= withGrain 6 >>= withMultipliable
        "millionen" -> double 1e6 >>= withGrain 6 >>= withMultipliable
        _           -> Nothing
      _ -> Nothing
  }

zeroNineteenMap :: HashMap Text Integer
zeroNineteenMap = HashMap.fromList
  [ ("keine", 0)
  , ("null", 0)
  , ("nichts", 0)
  , ("keiner", 0)
  , ("kein", 0)
  , ("keins", 0)
  , ("keinen", 0)
  , ("keines", 0)
  , ("einer", 1)
  , ("eins", 1)
  , ("ein", 1)
  , ("eine", 1)
  , ("einser", 1)
  , ("zwei", 2)
  , ("drei", 3)
  , ("vier", 4)
  , ("fünf", 5)
  , ("sechs", 6)
  , ("sieben", 7)
  , ("acht", 8)
  , ("neun", 9)
  , ("zehn", 10)
  , ("elf", 11)
  , ("zwölf", 12)
  , ("dreizehn", 13)
  , ("vierzehn", 14)
  , ("fünfzehn", 15)
  , ("sechzehn", 16)
  , ("siebzehn", 17)
  , ("achtzehn", 18)
  , ("neunzehn", 19)
  ]

-- TODO: Single-word composition (#110)
ruleZeroToNineteen :: Rule
ruleZeroToNineteen = Rule
  { name = "integer (0..19)"
  , pattern =
    [ regex "(keine[rn]|keine?s?|null|nichts|eins?(er)?|zwei|dreizehn|drei|vierzehn|vier|fünfzehn|fünf|sechzehn|sechs|siebzehn|sieben|achtzehn|acht|neunzehn|neun|elf|zwölf)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) zeroNineteenMap >>= integer
      _ -> Nothing
  }

tensMap :: HashMap Text Integer
tensMap = HashMap.fromList
  [ ( "zwanzig" , 20 )
  , ( "dreissig", 30 )
  , ( "vierzig" , 40 )
  , ( "fünfzig" , 50 )
  , ( "sechzig" , 60 )
  , ( "siebzig" , 70 )
  , ( "achtzig" , 80 )
  , ( "neunzig" , 90 )
  ]

-- TODO: Single-word composition (#110)
ruleInteger2 :: Rule
ruleInteger2 = Rule
  { name = "integer (20..90)"
  , pattern =
    [ regex "(zwanzig|dreissig|vierzig|fünfzig|sechzig|siebzig|achtzig|neunzig)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) tensMap >>= integer
      _ -> Nothing
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

rules :: [Rule]
rules =
  [ ruleCouple
  , ruleDecimalNumeral
  , ruleDecimalWithThousandsSeparator
  , ruleDozen
  , ruleFew
  , ruleInteger2
  , ruleInteger3
  , ruleIntegerWithThousandsSeparator
  , ruleIntersect
  , ruleMultiply
  , ruleNumeralDotNumeral
  , ruleNumeralsPrefixWithNegativeOrMinus
  , ruleNumeralsSuffixesKMG
  , ruleNumeralsUnd
  , rulePowersOfTen
  , ruleTen
  , ruleZeroToNineteen
  ]
