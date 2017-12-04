-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


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

ruleIntegers :: Rule
ruleIntegers = Rule
  { name = "integer (numeric)"
  , pattern =
    [ regex "(\\d{1,18})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        toInteger <$> parseInt match >>= integer
      _ -> Nothing
  }

ruleDozen :: Rule
ruleDozen = Rule
  { name = "a dozen of"
  , pattern =
    [ regex "(uma )?d(u|ú)zia?( de)?"
    ]
  , prod = \_ -> integer 12 >>= withMultipliable >>= notOkForAnyTime
  }

zeroNineteenMap :: HashMap Text Integer
zeroNineteenMap = HashMap.fromList
  [ ( "zero"        , 0 )
  , ( "um"          , 1 )
  , ( "dois"        , 2 )
  , ( "tres"        , 3 )
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
  , ( "quatorze"    , 14 )
  , ( "quinze"      , 15 )
  , ( "dezesseis"   , 16 )
  , ( "dezessete"   , 17 )
  , ( "dezoito"     , 18 )
  , ( "dezenove"    , 19 )
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
  -- e.g. fourteen must be before four, otherwise four will always shadow fourteen
  , pattern =
    [ regex "(zero|um|(dois|duas)|(um |uma )?(par)(es)?( de)?|tr(e|ê)s|(um )?pouco|quatorze|quatro|quinze|cinco|dezesseis|seis|dezessete|sete|dezoito|oito|dezenove|nove|dez|onze|doze|treze)"
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
  , ( "cinquenta" , 50 )
  , ( "sessenta"  , 60 )
  , ( "setenta"   , 70 )
  , ( "oitenta"   , 80 )
  , ( "noventa"   , 90 )
  ]

ruleTens :: Rule
ruleTens = Rule
  { name = "integer (20..90)"
  , pattern =
    [ regex "(vinte|trinta|quarenta|cinquenta|sessenta|setenta|oitenta|noventa)"
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
  { name = "integer (100..900)"
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
        "milhao"   -> double 1e6 >>= withGrain 6 >>= withMultipliable
        "milhão"   -> double 1e6 >>= withGrain 6 >>= withMultipliable
        "milhões"  -> double 1e6 >>= withGrain 6 >>= withMultipliable
        "milhoes"  -> double 1e6 >>= withGrain 6 >>= withMultipliable
        "bilhao"   -> double 1e9 >>= withGrain 9 >>= withMultipliable
        "bilhão"   -> double 1e9 >>= withGrain 9 >>= withMultipliable
        "bilhões"  -> double 1e9 >>= withGrain 9 >>= withMultipliable
        "bilhoes"  -> double 1e9 >>= withGrain 9 >>= withMultipliable
        "mil"      -> double 1e3 >>= withGrain 3 >>= withMultipliable
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
      (Token Numeral (NumeralData {TNumeral.value = tens}):
       Token Numeral (NumeralData {TNumeral.value = units}):
       _) -> double $ tens + units
      _ -> Nothing
  }

ruleCompositeCents :: Rule
ruleCompositeCents = Rule
  { name = "integer 100..999"
  , pattern =
    [ oneOf [100, 200..900]
    , numberBetween 1 100
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = tens}):
       Token Numeral (NumeralData {TNumeral.value = units}):
       _) -> double $ tens + units
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
      (Token Numeral (NumeralData {TNumeral.value = hundreds}):
       Token Numeral (NumeralData {TNumeral.value = rest}):
       _) -> double $ hundreds*100 + rest
      _ -> Nothing
  }

ruleDotSpelledOut :: Rule
ruleDotSpelledOut = Rule
  { name = "one point 2"
  , pattern =
    [ dimension Numeral
    , regex "point|dot"
    , numberWith TNumeral.grain isNothing
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
    , numberWith TNumeral.grain isNothing
    ]
  , prod = \tokens -> case tokens of
      (_:Token Numeral nd:_) -> double . decimalsToDouble $ TNumeral.value nd
      _ -> Nothing
  }

ruleDecimals :: Rule
ruleDecimals = Rule
  { name = "decimal number"
  , pattern =
    [ regex "(\\d*\\.\\d+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> parseDecimal True match
      _ -> Nothing
  }

ruleFractions :: Rule
ruleFractions = Rule
  { name = "fractional number"
  , pattern =
    [ regex "(\\d+)/(\\d+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (numerator:denominator:_)):_) -> do
        n <- parseDecimal False numerator
        d <- parseDecimal False denominator
        divide n d
      _ -> Nothing
  }

ruleCommas :: Rule
ruleCommas = Rule
  { name = "comma-separated numbers"
  , pattern =
    [ regex "(\\d+(,\\d\\d\\d)+(\\.\\d+)?)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        parseDouble (Text.replace "," Text.empty match) >>= double
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
      (Token Numeral nd : Token RegexMatch (GroupMatch (match : _)):_) -> do
        x <- case Text.toLower match of
          "k" -> Just 1e3
          "m" -> Just 1e6
          "g" -> Just 1e9
          _ -> Nothing
        double $ TNumeral.value nd * x
      _ -> Nothing
  }

ruleNegative :: Rule
ruleNegative = Rule
  { name = "negative numbers"
  , pattern =
    [ regex "(-|menos|negativo)(?!\\s*-)"
    , numberWith TNumeral.value (>0)
    ]
  , prod = \tokens -> case tokens of
      (_:Token Numeral nd:_) -> double (TNumeral.value nd * (-1))
      _ -> Nothing
  }

ruleSum :: Rule
ruleSum = Rule
  { name = "intersect 2 numbers"
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

ruleDecsAnd :: Rule
ruleDecsAnd = Rule
  { name = "number (21..29 31..39 41..49 51..59 61..69 71..79 81..89 91..99)"
  , pattern =
    [ oneOf [20, 30, 40, 50, 60, 70, 80, 90]
    , regex "e"
    , numberBetween 1 10
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = v1}):
       _:
       Token Numeral (NumeralData {TNumeral.value = v2}):
       _) -> double $ v1 + v2
      _ -> Nothing
  }

ruleCentsAnd :: Rule
ruleCentsAnd = Rule
  { name = "number (100..199 200..299 300..399 400..499 500..599 600..699 700..799 800..899 900..999)"
  , pattern =
    [ oneOf [100, 200, 300, 400, 500, 600, 700, 800, 900]
    , regex "e"
    , numberBetween 1 99
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = v1}):
       _:
       Token Numeral (NumeralData {TNumeral.value = v2}):
       _) -> double $ v1 + v2
      _ -> Nothing
  }

ruleSumAnd :: Rule
ruleSumAnd = Rule
  { name = "intersect 2 numbers (with and)"
  , pattern =
    [ numberWith (fromMaybe 0 . TNumeral.grain) (>1)
    , regex "e"
    , numberWith TNumeral.multipliable not
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = val1, TNumeral.grain = Just g}):
       _:
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

rules :: [Rule]
rules =
  [ ruleIntegers
  , ruleToNineteen
  , ruleTens
  , ruleCent
  , rulePowersOfTen
  , ruleCompositeTens
  , ruleCompositeCents
  , ruleSkipHundreds
  , ruleDotSpelledOut
  , ruleLeadingDotSpelledOut
  , ruleDecimals
  , ruleFractions
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
