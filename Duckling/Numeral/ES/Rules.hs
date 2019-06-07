-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Numeral.ES.Rules
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
    [ regex "(una )?docena?( de)?"
    ]
  , prod = \_ -> integer 12 >>= withMultipliable >>= notOkForAnyTime
  }

zeroNineteenMap :: HashMap Text Integer
zeroNineteenMap = HashMap.fromList
  [ ( "zero" , 0 )
  , ( "cero" , 0 )
  , ( "un" , 1 )
  , ( "una" , 1 )
  , ( "uno" , 1 )
  , ( "dos" , 2 )
  , ( "trés" , 3 )
  , ( "tres" , 3 )
  , ( "cuatro" , 4 )
  , ( "cinco" , 5 )
  , ( "seis" , 6 )
  , ( "séis" , 6 )
  , ( "siete" , 7 )
  , ( "ocho" , 8 )
  , ( "nueve" , 9 )
  , ( "diez" , 10 )
  , ( "dies" , 10 )
  , ( "once" , 11 )
  , ( "doce" , 12 )
  , ( "trece" , 13 )
  , ( "catorce" , 14 )
  , ( "quince" , 15 )
  , ( "dieciseis" , 16 )
  , ( "diesiséis" , 16 )
  , ( "diesiseis" , 16 )
  , ( "dieciséis" , 16 )
  , ( "diecisiete" , 17 )
  , ( "dieciocho" , 18 )
  , ( "diecinueve" , 19 )
  ]

informalMap :: HashMap Text Integer
informalMap = HashMap.fromList
  [ ( "un par"      , 2 )
  , ( "un par de"   , 2 )
  , ( "par"         , 2 )
  , ( "pares"       , 2 )
  , ( "par de"      , 2 )
  , ( "pares de"    , 2 )
  , ( "un trio"     , 3 )
  , ( "trio"       , 3 )
  ]

ruleToNineteen :: Rule
ruleToNineteen = Rule
  { name = "integer (0..19)"
  , pattern =
    [ regex "((c|z)ero|un(o|a)?|dos|tr(é|e)s|cuatro|cinco|s(e|é)is|siete|ocho|nueve|die(z|s)|once|doce|trece|catorce|quince|die(c|s)is(é|e)is|diecisiete|dieciocho|diecinueve)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        let x = Text.toLower match in
        (HashMap.lookup x zeroNineteenMap >>= integer) <|>
        (HashMap.lookup x informalMap >>= integer >>= notOkForAnyTime)
      _ -> Nothing
  }

twentyOneToTwentyNineMap :: HashMap.HashMap Text.Text Integer
twentyOneToTwentyNineMap = HashMap.fromList
  [ ( "veintiuno" , 21 )
  , ( "veintiuna" , 21 )
  , ( "veintidos" , 22 )
  , ( "veintidós" , 22 )
  , ( "veintitrés" , 23 )
  , ( "veintitres" , 23 )
  , ( "veinticuatro" , 24 )
  , ( "veinticinco" , 25 )
  , ( "veintiséis" , 26 )
  , ( "veintiseis" , 26 )
  , ( "veintisiete" , 27 )
  , ( "veintiocho" , 28 )
  , ( "veintinueve" , 29 )
  ]

ruleTwentyOneToTwentyNine :: Rule
ruleTwentyOneToTwentyNine = Rule
  { name = "number (21..29)"
  , pattern =
    [ regex "(veintiun(o|a)|veintid(o|ó)s|veintitr(é|e)s|veinticuatro|veinticinco|veintis(é|e)is|veintisiete|veintiocho|veintinueve|treinta)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) twentyOneToTwentyNineMap >>= integer
      _ -> Nothing
  }

tensMap :: HashMap Text Integer
tensMap = HashMap.fromList
  [ ( "veinte" , 20 )
  , ( "treinta" , 30 )
  , ( "cuarenta" , 40 )
  , ( "cincuenta" , 50 )
  , ( "sesenta" , 60 )
  , ( "setenta" , 70 )
  , ( "ochenta" , 80 )
  , ( "noventa" , 90 )
  ]

ruleTens :: Rule
ruleTens = Rule
  { name = "tens (20..90)"
  , pattern =
    [ regex "(veinte|treinta|cuarenta|cincuenta|sesenta|setenta|ochenta|noventa)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) tensMap >>= integer
      _ -> Nothing
  }

centsMap :: HashMap Text Integer
centsMap = HashMap.fromList
  [ ( "cien" , 100 )
  , ( "cientos" , 100 )
  , ( "ciento" , 100 )
  , ( "doscientos" , 200 )
  , ( "trescientos" , 300 )
  , ( "cuatrocientos" , 400 )
  , ( "quinientos" , 500 )
  , ( "seiscientos" , 600 )
  , ( "setecientos" , 700 )
  , ( "ochocientos" , 800 )
  , ( "novecientos" , 900 )
  ]

ruleCent :: Rule
ruleCent = Rule
  { name = "hundreds (100..900)"
  , pattern =
    [ regex "(cien(to)?s?|doscientos|trescientos|cuatrocientos|quinientos|seiscientos|setecientos|ochocientos|novecientos)"
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
    [ regex "(millón|millon|billón|billon|mil)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "mil"      -> double 1e3 >>= withGrain 3 >>= withMultipliable
        "millón"   -> double 1e6 >>= withGrain 6 >>= withMultipliable
        "millon"   -> double 1e6 >>= withGrain 6 >>= withMultipliable
        "billón"   -> double 1e9 >>= withGrain 9 >>= withMultipliable
        "billon"   -> double 1e9 >>= withGrain 9 >>= withMultipliable
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
  { name = "number (31..39 .. 91..99)"
  , pattern =
    [ oneOf [30..90]
    , regex "y"
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
    , regex "y"
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
    , regex "coma"
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
    [ regex "coma"
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
  , ruleTwentyOneToTwentyNine
  , ruleNegative
  , ruleSum
  , ruleDecsAnd
  , ruleCentsAnd
  , ruleSumAnd
  , ruleMultiply
  , ruleDozen
  ]
