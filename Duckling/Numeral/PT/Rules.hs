-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Numeral.PT.Rules
  ( rules
  ) where

import Control.Applicative ((<|>))
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

ruleDozen :: Rule
ruleDozen = Rule
  { name = "a dozen of"
  , pattern =
    [ regex "(uma )?d(u|ú)zias?( de)?"
    ]
  , prod = \_ -> integer 12 >>= withMultipliable >>= notOkForAnyTime
  }

zeroNineteenMap :: HashMap Text Integer
zeroNineteenMap = HashMap.fromList
  [ ( "zero"        , 0 )
  , ( "um"          , 1 )
  , ( "uma"         , 1 )
  , ( "dois"        , 2 )
  , ( "duas"        , 2 )
  , ( "tres"        , 3 )
  , ( "três"        , 3 )
  , ( "quatro"      , 4 )
  , ( "cinco"       , 5 )
  , ( "seis"        , 6 )
  , ( "sete"        , 7 )
  , ( "oito"        , 8 )
  , ( "nove"        , 9 )
  , ( "dez"         , 10 )
  , ( "onze"        , 11 )
  , ( "doze"        , 12 )
  , ( "treze"       , 13 )
  , ( "catorze"     , 14 )
  , ( "quatorze"    , 14 )
  , ( "quinze"      , 15 )
  , ( "dezesseis"   , 16 )
  , ( "dezasseis"   , 16 )
  , ( "dezessete"   , 17 )
  , ( "dezassete"   , 17 )
  , ( "dezoito"     , 18 )
  , ( "dezenove"    , 19 )
  , ( "dezanove"    , 19 )
  ]

informalMap :: HashMap Text Integer
informalMap = HashMap.fromList
  [ ( "um par"      , 2 )
  , ( "um par de"   , 2 )
  , ( "par"         , 2 )
  , ( "pares"       , 2 )
  , ( "par de"      , 2 )
  , ( "pares de"    , 2 )
  , ( "um pouco"    , 3 )
  , ( "pouco"       , 3 )
  ]

ruleToNineteen :: Rule
ruleToNineteen = Rule
  { name = "integer (0..19)"
  , pattern =
    [ regex "(zero|d(oi|ua)s|(uma? )?par(es)?( de)?|tr(e|ê)s|(um )?pouco|uma?|(c|qu)atorze|quatro|quinze|cinco|dez[ea]sseis|seis|dez[ea]ssete|sete|dezoito|oito|dez[ea]nove|nove|dez|onze|doze|treze)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        let x = Text.toLower match in
        (HashMap.lookup x zeroNineteenMap >>= integer) <|>
        (HashMap.lookup x informalMap >>= integer >>= notOkForAnyTime)
      _ -> Nothing
  }

tensMap :: HashMap Text Integer
tensMap = HashMap.fromList
  [ ( "vinte"     , 20 )
  , ( "trinta"    , 30 )
  , ( "quarenta"  , 40 )
  , ( "cincoenta" , 50 )
  , ( "cinquenta" , 50 )
  , ( "cinqüenta" , 50 )
  , ( "sessenta"  , 60 )
  , ( "setenta"   , 70 )
  , ( "oitenta"   , 80 )
  , ( "noventa"   , 90 )
  ]

ruleTens :: Rule
ruleTens = Rule
  { name = "tens (20..90)"
  , pattern =
    [ regex "(vinte|trinta|quarenta|cin(co|q[uü])enta|sessenta|setenta|oitenta|noventa)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) tensMap >>= integer
      _ -> Nothing
  }

centsMap :: HashMap Text Integer
centsMap = HashMap.fromList
  [ ( "cem"           , 100 )
  , ( "cento"         , 100 )
  , ( "duzentos"      , 200 )
  , ( "trezentos"     , 300 )
  , ( "quatrocentos"  , 400 )
  , ( "quinhetos"     , 500 )
  , ( "seiscentos"    , 600 )
  , ( "setecentos"    , 700 )
  , ( "oitocentos"    , 800 )
  , ( "novecentos"    , 900 )
  ]

ruleCent :: Rule
ruleCent = Rule
  { name = "hundreds (100..900)"
  , pattern =
    [ regex "(cem|cento|duzentos|trezentos|quatrocentos|quinhetos|seiscentos|setecentos|oitocentos|novecentos)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) centsMap >>= integer
      _ -> Nothing
  }

rulePowersOfTen :: Rule
rulePowersOfTen = Rule
  { name = "powers of tens"
  , pattern =
    [ regex "(milhao|milhão|milhões|milhoes|bilhao|bilhão|bilhões|bilhoes|mil)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "mil"      -> double 1e3 >>= withGrain 3 >>= withMultipliable
        "milhao"   -> double 1e6 >>= withGrain 6 >>= withMultipliable
        "milhão"   -> double 1e6 >>= withGrain 6 >>= withMultipliable
        "milhões"  -> double 1e6 >>= withGrain 6 >>= withMultipliable
        "milhoes"  -> double 1e6 >>= withGrain 6 >>= withMultipliable
        "bilhao"   -> double 1e9 >>= withGrain 9 >>= withMultipliable
        "bilhão"   -> double 1e9 >>= withGrain 9 >>= withMultipliable
        "bilhões"  -> double 1e9 >>= withGrain 9 >>= withMultipliable
        "bilhoes"  -> double 1e9 >>= withGrain 9 >>= withMultipliable
        _          -> Nothing
      _ -> Nothing
  }

ruleCompositeTens :: Rule
ruleCompositeTens = Rule
  { name = "integer 21..99"
  , pattern =
    [ oneOf [20,30..90]
    , numberBetween 1 10
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = tens}:
       Token Numeral NumeralData{TNumeral.value = units}:
       _) -> double $ tens + units
      _ -> Nothing
  }

ruleDecsAnd :: Rule
ruleDecsAnd = Rule
  { name = "number (21..29 31..39 .. 91..99)"
  , pattern =
    [ oneOf [20, 30..90]
    , regex "e"
    , numberBetween 1 10
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v1}:
       _:
       Token Numeral NumeralData{TNumeral.value = v2}:
       _) -> double $ v1 + v2
      _ -> Nothing
  }

ruleCompositeCents :: Rule
ruleCompositeCents = Rule
  { name = "integer 101..999"
  , pattern =
    [ oneOf [100, 200..900]
    , numberBetween 1 100
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = hundreds}:
       Token Numeral NumeralData{TNumeral.value = units}:
       _) -> double $ hundreds + units
      _ -> Nothing
  }

ruleCentsAnd :: Rule
ruleCentsAnd = Rule
  { name = "number (101..199 201..299 .. 901..999)"
  , pattern =
    [ oneOf [100, 200..900]
    , regex "e"
    , numberBetween 1 100
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v1}:
       _:
       Token Numeral NumeralData{TNumeral.value = v2}:
       _) -> double $ v1 + v2
      _ -> Nothing
  }

ruleSkipHundreds :: Rule
ruleSkipHundreds = Rule
  { name = "one twenty two"
  , pattern =
    [ numberBetween 1 10
    , numberBetween 10 100
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = hundreds}:
       Token Numeral NumeralData{TNumeral.value = rest}:
       _) -> double $ hundreds*100 + rest
      _ -> Nothing
  }

ruleDotSpelledOut :: Rule
ruleDotSpelledOut = Rule
  { name = "one point 2"
  , pattern =
    [ dimension Numeral
    , regex "ponto"
    , Predicate $ not . hasGrain
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral nd1:_:Token Numeral nd2:_) ->
        double $ TNumeral.value nd1 + decimalsToDouble (TNumeral.value nd2)
      _ -> Nothing
  }

ruleLeadingDotSpelledOut :: Rule
ruleLeadingDotSpelledOut = Rule
  { name = "point 77"
  , pattern =
    [ regex "ponto"
    , Predicate $ not . hasGrain
    ]
  , prod = \tokens -> case tokens of
      (_:Token Numeral nd:_) -> double . decimalsToDouble $ TNumeral.value nd
      _ -> Nothing
  }

ruleDecimals :: Rule
ruleDecimals = Rule
  { name = "decimal number"
  , pattern =
    [ regex "(\\d*\\,\\d+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> parseDecimal False match
      _ -> Nothing
  }

ruleCommas :: Rule
ruleCommas = Rule
  { name = "dot-separated numbers"
  , pattern =
    [ regex "(\\d+(\\.\\d\\d\\d)+(\\,\\d+)?)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        parseDecimal False $ Text.replace "." Text.empty match
      _ -> Nothing
  }

ruleSuffixes :: Rule
ruleSuffixes = Rule
  { name = "suffixes (K,M,G))"
  , pattern =
    [ dimension Numeral
    , regex "(k|m|g)(?=[\\W$€¢£]|$)"
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

ruleNegative :: Rule
ruleNegative = Rule
  { name = "negative numbers"
  , pattern =
    [ regex "(-|menos|negativo)(?!\\s*\\-)"
    , Predicate isPositive
    ]
  , prod = \tokens -> case tokens of
      (_:Token Numeral nd:_) -> double $ TNumeral.value nd * (-1)
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

ruleSumAnd :: Rule
ruleSumAnd = Rule
  { name = "intersect 2 numbers (with and)"
  , pattern =
    [ Predicate hasGrain
    , regex "e"
    , Predicate $ and . sequence [not . isMultipliable, isPositive]
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = val1, TNumeral.grain = Just g}:
       _:
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
  [ ruleToNineteen
  , ruleTens
  , ruleCent
  , rulePowersOfTen
  , ruleCompositeTens
  , ruleCompositeCents
  , ruleSkipHundreds
  , ruleDotSpelledOut
  , ruleLeadingDotSpelledOut
  , ruleDecimals
  , ruleCommas
  , ruleSuffixes
  , ruleNegative
  , ruleSum
  , ruleDecsAnd
  , ruleCentsAnd
  , ruleSumAnd
  , ruleMultiply
  , ruleDozen
  ]
