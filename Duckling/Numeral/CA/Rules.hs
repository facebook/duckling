-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.CA.Rules (rules) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Data.String
import Data.Text (Text)
import qualified Data.Text as Text
import Prelude

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers
import Duckling.Numeral.Types (NumeralData(..))
import qualified Duckling.Numeral.Types as TNumeral
import Duckling.Regex.Types
import Duckling.Types

zeroToFifteenMap :: HashMap Text Integer
zeroToFifteenMap =
  HashMap.fromList
    [ ("zero", 0)
    , ("u", 1)
    , ("un", 1)
    , ("una", 1)
    , ("dos", 2)
    , ("dues", 2)
    , ("tres", 3)
    , ("quatre", 4)
    , ("cinc", 5)
    , ("sis", 6)
    , ("set", 7)
    , ("vuit", 8)
    , ("nou", 9)
    , ("deu", 10)
    , ("onze", 11)
    , ("dotze", 12)
    , ("tretze", 13)
    , ("catorze", 14)
    , ("quinze", 15)
    ]

ruleZeroToFifteen :: Rule
ruleZeroToFifteen = Rule
  { name = "number (0..15)"
  , pattern =
      [ regex
          "(zero|u(na|n)?|d(o|ue)s|tres|quatre|cinc|sis|set|vuit|nou|deu|onze|dotze|tretze|catorze|quinze)"
      ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) zeroToFifteenMap >>= integer
      _ -> Nothing
  }

ruleNumeralsPrefixWithNegativeOrMinus :: Rule
ruleNumeralsPrefixWithNegativeOrMinus = Rule
  { name = "numbers prefix with -, negative or minus"
  , pattern = [regex "-|menys", Predicate isPositive]
  , prod = \case
      (_ : Token Numeral NumeralData { TNumeral.value = v } : _) ->
        double $ negate v
      _ -> Nothing
  }

tensMap :: HashMap Text Integer
tensMap =
  HashMap.fromList
    [ ("vint", 20)
    , ("trenta", 30)
    , ("quaranta", 40)
    , ("cinquanta", 50)
    , ("seixanta", 60)
    , ("setanta", 70)
    , ("vuitanta", 80)
    , ("noranta", 90)
    ]

ruleTens :: Rule
ruleTens = Rule
  { name = "number (20..90)"
  , pattern =
      [ regex
          "(vint|(tre|quara|cinqua|seixa|seta|vuita|nora)nta)"
      ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)) : _) ->
        HashMap.lookup (Text.toLower match) tensMap >>= integer
      _ -> Nothing
  }

sixteenToTwentyNineMap :: HashMap Text Integer
sixteenToTwentyNineMap =
  HashMap.fromList
    [ ("setze", 16)
    , ("disset", 17)
    , ("dèsset", 17)
    , ("devuit", 18)
    , ("divuit", 18)
    , ("dihuit", 18)
    , ("dinou", 19)
    , ("dènou", 19)
    , ("denou", 19)
    , ("vint-i-u", 21)
    , ("vint-i-una", 21)
    , ("vint-i-dos", 22)
    , ("vint-i-tres", 23)
    , ("vint-i-quatre", 24)
    , ("vint-i-cinc", 25)
    , ("vint-i-sis", 26)
    , ("vint-i-set", 27)
    , ("vint-i-vuit", 28)
    , ("vint-i-nou", 29)
    ]

ruleLowerTensWithOnes :: Rule
ruleLowerTensWithOnes = Rule
  { name = "number (16..19 21..29)"
  , pattern =
      [ regex
          "(setze|d(i|e|è)sset|d(e|i)(v|h)uit|d(i|e|è)nou|vint-i-u(na)?|vint-i-dos|vint-i-tres|vint-i-quatre|vint-i-cinc|vint-i-sis|vint-i-set|vint-i-vuit|vint-i-nou)"
      ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) sixteenToTwentyNineMap >>= integer
      _ -> Nothing
  }

ruleHigherTensWithOnes :: Rule
ruleHigherTensWithOnes = Rule
  { name = "number (31..39 41..49 51..59 61..69 71..79 81..89 91..99)"
  , pattern =
      [oneOf [30, 40, 50, 60, 70, 80, 90], regex "-", numberBetween 1 9]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = v1}:
       _:
       Token Numeral NumeralData{TNumeral.value = v2}:
       _) -> double $ v1 + v2
      _ -> Nothing
  }

ruleNumeralsSuffixesKMG :: Rule
ruleNumeralsSuffixesKMG = Rule
  { name = "numbers suffixes (K, M, G)"
  , pattern = [dimension Numeral, regex "([kmg])(?=[\\W\\$€]|$)"]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = v}:
       Token RegexMatch (GroupMatch (match:_)):
       _) ->
        case Text.toLower match of
          "k" -> double $ v * 1e3
          "m" -> double $ v * 1e6
          "g" -> double $ v * 1e9
          _ -> Nothing
      _ -> Nothing
  }

oneHundredToThousandMap :: HashMap Text Integer
oneHundredToThousandMap =
  HashMap.fromList
    [ ("cent", 100)
    , ("cents", 100)
    , ("dos-cents", 200)
    , ("tres-cents", 300)
    , ("quatre-cents", 400)
    , ("cinc-cents", 500)
    , ("sis-cents", 600)
    , ("set-cents", 700)
    , ("vuit-cents", 800)
    , ("nou-cents", 900)
    , ("mil", 1000)
    ]

ruleTwenties :: Rule
ruleTwenties = Rule
  { name = "number (21..29)"
  , pattern =
      [oneOf [20], regex "(-i-| i )", numberBetween 1 10]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = v1}:
       _:
       Token Numeral NumeralData{TNumeral.value = v2}:
       _) -> double $ v1 + v2
      _ -> Nothing
  }

ruleHundreds :: Rule
ruleHundreds = Rule
  { name = "number 100..1000 "
  , pattern =
      [ regex
          "(cent(s)?|dos-cents|tres-cents|quatre-cents|cinc-cents|sis-cents|set-cents|vuit-cents|nou-cents|mil)"
      ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) oneHundredToThousandMap >>= integer
      _ -> Nothing
  }

-- Afegeixo regex "-" perque les centenes s'escriuen dos-cent, tres-cent
ruleNumerals :: Rule
ruleNumerals = Rule
  { name = "numbers 200..999"
  , pattern =
      [ numberBetween 2 10
      , regex "-"
      , numberWith TNumeral.value (== 100)
      , numberBetween 0 100
      ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = v1}:
       _:
       Token Numeral NumeralData{TNumeral.value = v2}:
       _) -> double $ 100 * v1 + v2
      _ -> Nothing
  }

ruleNumeralDotNumeral :: Rule
ruleNumeralDotNumeral = Rule
  { name = "number dot number"
  , pattern = [dimension Numeral, regex "coma", Predicate $ not . hasGrain]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = v1}:
       _:
       Token Numeral NumeralData{TNumeral.value = v2}:
       _) -> double $ v1 + decimalsToDouble v2
      _ -> Nothing
  }

ruleBelowTenWithTwoDigits :: Rule
ruleBelowTenWithTwoDigits = Rule
  { name = "integer (0-9) with two digits"
  , pattern =
      [ regex "zero|0"
      , numberBetween 1 10
      ]
  , prod = \case
      (_:Token Numeral NumeralData{TNumeral.value = v}:_) -> double v
      _ -> Nothing
  }

ruleDecimalWithThousandsSeparator :: Rule
ruleDecimalWithThousandsSeparator = Rule
  { name = "decimal with thousands separator ."
  , pattern = [regex "(\\d+(\\.\\d\\d\\d)+,\\d+)"]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        let fmt = Text.replace "," "." . Text.replace "." Text.empty $ match
        in parseDouble fmt >>= double
      _ -> Nothing
  }

ruleDecimalNumeral :: Rule
ruleDecimalNumeral = Rule
  { name = "decimal number ,"
  , pattern = [regex "(\\d*,\\d+)"]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        parseDecimal False match
      _ -> Nothing
  }

ruleIntegerWithThousandsSeparator :: Rule
ruleIntegerWithThousandsSeparator = Rule
  { name = "integer with thousands separator ."
  , pattern = [regex "(\\d{1,3}(\\.\\d\\d\\d){1,5})"]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        parseDouble (Text.replace "." Text.empty match) >>= double
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleBelowTenWithTwoDigits
  , ruleZeroToFifteen
  , ruleTens
  , ruleTwenties
  , ruleLowerTensWithOnes
  , ruleHigherTensWithOnes
  , ruleHundreds

  , ruleNumeralDotNumeral
  , ruleNumerals
  , ruleNumeralsPrefixWithNegativeOrMinus
  , ruleNumeralsSuffixesKMG
  , ruleDecimalNumeral
  , ruleDecimalWithThousandsSeparator
  , ruleIntegerWithThousandsSeparator
  ]
