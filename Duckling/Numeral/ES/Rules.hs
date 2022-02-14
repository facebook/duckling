-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Duckling.Numeral.ES.Rules (rules) where

import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Data.String
import qualified Data.Text as Text
import Prelude

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers
import Duckling.Numeral.Types (NumeralData(..))
import qualified Duckling.Numeral.Types as TNumeral
import Duckling.Regex.Types
import Duckling.Types

zeroToFifteenMap :: HashMap.HashMap Text.Text Integer
zeroToFifteenMap =
  HashMap.fromList
    [ ("zero", 0)
    , ("cero", 0)
    , ("un", 1)
    , ("una", 1)
    , ("uno", 1)
    , ("dos", 2)
    , ("trés", 3)
    , ("tres", 3)
    , ("cuatro", 4)
    , ("cinco", 5)
    , ("seis", 6)
    , ("séis", 6)
    , ("siete", 7)
    , ("ocho", 8)
    , ("nueve", 9)
    , ("diez", 10)
    , ("dies", 10)
    , ("once", 11)
    , ("doce", 12)
    , ("trece", 13)
    , ("catorce", 14)
    , ("quince", 15)
    ]

ruleNumeralZeroToFifteen :: Rule
ruleNumeralZeroToFifteen = Rule
  { name = "number (0..15)"
  , pattern =
      [ regex
          "((c|z)ero|un(o|a)?|dos|tr(é|e)s|cuatro|cinco|s(e|é)is|siete|ocho|nueve|die(z|s)|once|doce|trece|catorce|quince)"
      ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match : _)) : _) ->
        HashMap.lookup (Text.toLower match) zeroToFifteenMap >>= integer
      _ -> Nothing
  }

ruleBelowTenWithTwoDigits :: Rule
ruleBelowTenWithTwoDigits = Rule
  {
    name = "integer (0-9) with two digits"
  , pattern =
      [ regex "((c|z)ero)|0"
      , Predicate $ numberBetween 1 10
      ]
  , prod = \case
      (
        _:
        Token Numeral NumeralData { TNumeral.value = v }:
        _
        ) -> double v
      _ -> Nothing
  }

sixteenToTwentyNineMap :: HashMap.HashMap Text.Text Integer
sixteenToTwentyNineMap =
  HashMap.fromList
    [ ("dieciseis", 16)
    , ("diesiséis", 16)
    , ("diesiseis", 16)
    , ("dieciséis", 16)
    , ("diecisiete", 17)
    , ("dieciocho", 18)
    , ("diecinueve", 19)
    , ("veintiuno", 21)
    , ("veintiuna", 21)
    , ("veintidos", 22)
    , ("veintidós", 22)
    , ("veintitrés", 23)
    , ("veintitres", 23)
    , ("veinticuatro", 24)
    , ("veinticinco", 25)
    , ("veintiséis", 26)
    , ("veintiseis", 26)
    , ("veintisiete", 27)
    , ("veintiocho", 28)
    , ("veintinueve", 29)
    ]


ruleNumeralSixteenToTwentyNine :: Rule
ruleNumeralSixteenToTwentyNine = Rule
  { name = "number (16..19 21..29)"
  , pattern =
      [ regex
          "(die(c|s)is(é|e)is|diecisiete|dieciocho|diecinueve|veintiun(o|a)|veintid(o|ó)s|veintitr(é|e)s|veinticuatro|veinticinco|veintis(é|e)is|veintisiete|veintiocho|veintinueve|treinta)"
      ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match : _)) : _) ->
        HashMap.lookup (Text.toLower match) sixteenToTwentyNineMap >>= integer
      _ -> Nothing
  }

ruleNumeralSixteenToNineteenWithDiez :: Rule
ruleNumeralSixteenToNineteenWithDiez = Rule
  { name = "number (16..19, two words)"
  , pattern =
      [ numberWith TNumeral.value (== 10)
      , regex "y"
      , Predicate $ numberBetween 6 10
      ]
  , prod = \case
      (_ : _ : Token Numeral NumeralData { TNumeral.value = v } : _) ->
        double $ 10 + v
      _ -> Nothing
  }

byTensMap :: HashMap.HashMap Text.Text Integer
byTensMap =
  HashMap.fromList
    [ ("veinte", 20)
    , ("treinta", 30)
    , ("cuarenta", 40)
    , ("cincuenta", 50)
    , ("sesenta", 60)
    , ("setenta", 70)
    , ("ochenta", 80)
    , ("noventa", 90)
    ]


ruleNumeralTwentyToNinetyTens :: Rule
ruleNumeralTwentyToNinetyTens = Rule
  { name = "number (20..90)"
  , pattern =
      [ regex
          "(veinte|treinta|cuarenta|cincuenta|sesenta|setenta|ochenta|noventa)"
      ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match : _)) : _) ->
        HashMap.lookup (Text.toLower match) byTensMap >>= integer
      _ -> Nothing
  }

ruleNumeralTwentyOneToNinetyNine :: Rule
ruleNumeralTwentyOneToNinetyNine = Rule
  { name = "number (21..29 31..39 41..49 51..59 61..69 71..79 81..89 91..99)"
  , pattern =
      [ oneOf [20, 30 .. 90]
      , regex "y"
      , Predicate $ numberBetween 1 10
      ]
  , prod = \case
      (Token Numeral NumeralData { TNumeral.value = v1 } : _ : Token Numeral NumeralData { TNumeral.value = v2 } : _) ->
        double $ v1 + v2
      _ -> Nothing
  }

bigNumbersMap :: HashMap.HashMap Text.Text Integer
bigNumbersMap =
  HashMap.fromList
    [ ("cien", 100)
    , ("cientos", 100)
    , ("ciento", 100)
    , ("doscientos", 200)
    , ("trescientos", 300)
    , ("cuatrocientos", 400)
    , ("quinientos", 500)
    , ("seiscientos", 600)
    , ("setecientos", 700)
    , ("ochocientos", 800)
    , ("novecientos", 900)
    , ("mil", 1000)
    , ("millon", 1000000)
    , ("millón", 1000000)
    , ("un millon", 1000000)
    , ("un millón", 1000000)
    , ("millones", 1000000)
    -- Note: billion and larger is ambiguous becaouse of long vs short scale
    ]

ruleBigNumeral :: Rule
ruleBigNumeral = Rule
  { name = "big number 100 to 1K"
  , pattern =
      [ regex
          "(cien(to|tos)?|doscientos|trescientos|cuatrocientos|quinientos|seiscientos|setecientos|ochocientos|novecientos|(un )?mill(o|ó)n)"
      ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match : _)) : _) ->
        HashMap.lookup (Text.toLower match) bigNumbersMap >>= integer
      _ -> Nothing
  }

ruleBigNumeralMultipliable :: Rule
ruleBigNumeralMultipliable = Rule
  { name = "1K or 1M in multipliable form"
  , pattern =
      [ regex
          "(mil(lones)?)"
      ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match : _)) : _) ->
        HashMap.lookup (Text.toLower match) bigNumbersMap >>= integer >>= withMultipliable
      _ -> Nothing
  }

ruleTwoPartHundreds :: Rule
ruleTwoPartHundreds = Rule
 { name = "2..9 cientos"
 , pattern =
     [ Predicate $ numberBetween 2 10
     , regex "cientos"
     ]
  , prod = \case
      (Token Numeral NumeralData { TNumeral.value = v1 } : _ : _) ->
        double $ 100 * v1
      _ -> Nothing
  }

ruleNumeralHundredsAndSmaller :: Rule
ruleNumeralHundredsAndSmaller = Rule
  { name = "<hundreds> 0..99"
  , pattern =
      [ numberWith TNumeral.value (TNumeral.isMultiple 100)
      , Predicate $ numberBetween 0 100
      ]
  , prod = \case
      (Token Numeral NumeralData { TNumeral.value = v1 } : Token Numeral NumeralData { TNumeral.value = v2 } : _)
        | v1 > 0 && v1 < 1000 -> double $ v1 + v2
      _ -> Nothing
  }

ruleNumeralMultiply :: Rule
ruleNumeralMultiply = Rule
  { name = "2..999 <multipliable>"
  , pattern =
      [ Predicate $ numberBetween 2 1000
      , Predicate isMultipliable
      ]
  , prod = \case
      (Token Numeral NumeralData { TNumeral.value = v1 } : Token Numeral NumeralData { TNumeral.value = v2 } : _) ->
        double $ v1 * v2
      _ -> Nothing
  }

ruleNumeralThousandsAnd :: Rule
ruleNumeralThousandsAnd = Rule
  { name = "<thousands> 0..999"
  , pattern =
      [ numberWith TNumeral.value (TNumeral.isMultiple 1000)
      , Predicate $ numberBetween 0 999
      ]
  , prod = \case
      (Token Numeral NumeralData { TNumeral.value = v1 } : Token Numeral NumeralData { TNumeral.value = v2 } : _)
       | 0 < v1 && v1 < 1000000 -> double $ v1 + v2
      _ -> Nothing
  }

ruleNumeralMillionsAnd :: Rule
ruleNumeralMillionsAnd = Rule
  { name = "<millions> 0..999999"
  , pattern =
      [ numberWith TNumeral.value (TNumeral.isMultiple 1000000)
      , Predicate $ numberBetween 0 999999
      ]
  , prod = \case
      (Token Numeral NumeralData { TNumeral.value = v1 } : Token Numeral NumeralData { TNumeral.value = v2 } : _)
       | 0 < v1 -> double $ v1 + v2
      _ -> Nothing
  }

ruleNumeralDotNumeral :: Rule
ruleNumeralDotNumeral = Rule
  { name = "number dot number"
  , pattern = [dimension Numeral, regex "(co(n|ma)|punto)", Predicate $ not . hasGrain]
  , prod = \case
      (Token Numeral NumeralData { TNumeral.value = v1 } : _ : Token Numeral NumeralData { TNumeral.value = v2 } : _) ->
        double $ v1 + decimalsToDouble v2
      _ -> Nothing
  }

ruleLeadingDotNumeral :: Rule
ruleLeadingDotNumeral = Rule
  { name = "dot number"
  , pattern = [regex "coma|punto", Predicate $ not . hasGrain]
  , prod = \case
      (_:Token Numeral NumeralData{TNumeral.value = v}:_) ->
        double $ decimalsToDouble v
      _ -> Nothing
  }

ruleNumeralsSuffixesKMG :: Rule
ruleNumeralsSuffixesKMG = Rule
  { name = "numbers suffixes (K, M, G)"
  , pattern = [dimension Numeral, regex "([kmg])(?=[\\W\\$€]|$)"]
  , prod = \case
      (Token Numeral NumeralData { TNumeral.value = v } : Token RegexMatch (GroupMatch (match : _)) : _) ->
        case Text.toLower match of
          "k" -> double $ v * 1e3
          "m" -> double $ v * 1e6
          "g" -> double $ v * 1e9
          _ -> Nothing
      _ -> Nothing
  }

ruleNumeralsPrefixWithNegativeOrMinus :: Rule
ruleNumeralsPrefixWithNegativeOrMinus = Rule
  { name = "numbers prefix with -, negative or minus"
  , pattern = [regex "-|menos|negativ(o|a)", Predicate isPositive]
  , prod = \case
      (_ : Token Numeral NumeralData { TNumeral.value = v } : _) ->
        double $ v * (-1)
      _ -> Nothing
  }


rules :: [Rule]
rules =
  [ ruleNumeralZeroToFifteen
  , ruleBelowTenWithTwoDigits
  , ruleNumeralSixteenToTwentyNine
  , ruleNumeralSixteenToNineteenWithDiez
  , ruleNumeralTwentyToNinetyTens
  , ruleNumeralTwentyOneToNinetyNine
  , ruleBigNumeral
  , ruleBigNumeralMultipliable
  , ruleNumeralMultiply
  , ruleNumeralThousandsAnd
  , ruleNumeralMillionsAnd
  , ruleTwoPartHundreds
  , ruleNumeralHundredsAndSmaller
  , ruleNumeralDotNumeral
  , ruleLeadingDotNumeral
  , ruleNumeralsSuffixesKMG
  , ruleNumeralsPrefixWithNegativeOrMinus
  ]
