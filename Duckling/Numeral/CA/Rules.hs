-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Duckling.Numeral.CA.Rules (rules) where

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

ruleNumeralsPrefixWithNegativeOrMinus :: Rule
ruleNumeralsPrefixWithNegativeOrMinus = Rule
  { name = "numbers prefix with -, negative or minus"
  , pattern = [regex "-|menys", Predicate isPositive]
  , prod = \tokens -> case tokens of
      (_ : Token Numeral NumeralData { TNumeral.value = v } : _) ->
        double $ v * (-1)
      _ -> Nothing
  }

-- Ull, si a la següent no comença per 20 aquí tampoc
byTensMap :: HashMap.HashMap Text.Text Integer
byTensMap =
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


-- Ull, no tinc clar que hagi de començar per 20 donat que la vintena no s'escriu igual que de la 
--   trentena en amunt vint-i-u trenta-u

-- fico les variantas de curanta (incorrecte) i huitanta (correcte però no habitual)
ruleNumeral2 :: Rule
ruleNumeral2 = Rule
  { name = "number (20..90)"
  , pattern =
      [ regex
          "(vint|trenta|quaranta|cinquanta|seixanta|setanta|vuitanta|noranta)"
      ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match : _)) : _) ->
        HashMap.lookup (Text.toLower match) byTensMap >>= integer
      _ -> Nothing
  }

zeroToFifteenMap :: HashMap.HashMap Text.Text Integer
zeroToFifteenMap =
  HashMap.fromList
    [ ("zero", 0)
    , ("cero", 0)
    , ("u", 1)
    , ("una", 1)
    , ("un", 1)
    , ("dos", 2)
    , ("tres", 3)
    , ("quatre", 4)
    , ("cinco", 5)
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

ruleNumeral :: Rule
ruleNumeral = Rule
  { name = "number (0..15)"
  , pattern =
      [ regex
          "((c|z)ero|u(n|na)?|dos|tres|quatre|cinc|sis|set|vuit|nou|deu|onze|dotze|tretze|catorze|quinze)"
      ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match : _)) : _) ->
        HashMap.lookup (Text.toLower match) zeroToFifteenMap >>= integer
      _ -> Nothing
  }

sixteenToTwentyNineMap :: HashMap.HashMap Text.Text Integer
sixteenToTwentyNineMap =
  HashMap.fromList
    [ ("setze", 16)
    , ("disset", 17)
    , ("divuit", 18)
    , ("dinou", 19)
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

ruleNumeral5 :: Rule
ruleNumeral5 = Rule
  { name = "number (16..19 21..29)"
  , pattern =
      [ regex
          "(setze|disset|divuit|dinou|vint-i-u(na)?|vint-i-dos|vint-i-tres|vint-i-quatre|vint-i-cinc|vint-i-sis|vint-i-set|vint-i-vuit|vint-i-nou)"
      ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match : _)) : _) ->
        HashMap.lookup (Text.toLower match) sixteenToTwentyNineMap >>= integer
      _ -> Nothing
  }

-- Aquesta no aplica donat que no es composa com el castellà diez y siete, sino que te nom propi
-- es pot esborrar directament a la brava?
{- 
ruleNumeral3 :: Rule
ruleNumeral3 = Rule
  { name = "number (16..19)"
  , pattern = [numberWith TNumeral.value (== 10), regex "y", numberBetween 6 10]
  , prod = \tokens -> case tokens of
      (_ : _ : Token Numeral NumeralData { TNumeral.value = v } : _) ->
        double $ 10 + v
      _ -> Nothing
  }
   -}

ruleNumeralsSuffixesKMG :: Rule
ruleNumeralsSuffixesKMG = Rule
  { name = "numbers suffixes (K, M, G)"
  , pattern = [dimension Numeral, regex "([kmg])(?=[\\W\\$€]|$)"]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData { TNumeral.value = v } : Token RegexMatch (GroupMatch (match : _)) : _) ->
        case Text.toLower match of
          "k" -> double $ v * 1e3
          "m" -> double $ v * 1e6
          "g" -> double $ v * 1e9
          _ -> Nothing
      _ -> Nothing
  }

oneHundredToThousandMap :: HashMap.HashMap Text.Text Integer
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

ruleNumeral6 :: Rule
ruleNumeral6 = Rule
  { name = "number 100..1000 "
  , pattern =
      [ regex
          "(cent(s)?|dos-cents|tres-cents|quatre-cents|cinc-cents|sis-cents|set-cents|vuit-cents|nou-cents|mil)"
      ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match : _)) : _) ->
        HashMap.lookup (Text.toLower match) oneHundredToThousandMap >>= integer
      _ -> Nothing
  }

-- S'ha de dividir en dos del 21 al 29 unit per '-i-' 
--    i de la trentena en endevant unit per '-'
{- 
ruleNumeral4 :: Rule
ruleNumeral4 = Rule
  { name = "number (21..29 31..39 41..49 51..59 61..69 71..79 81..89 91..99)"
  , pattern =
      [oneOf [70, 20, 60, 50, 40, 90, 30, 80], regex "y", numberBetween 1 10]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData { TNumeral.value = v1 } : _ : Token Numeral NumeralData { TNumeral.value = v2 } : _) ->
        double $ v1 + v2
      _ -> Nothing
  } -}

ruleNumeral4 :: Rule
ruleNumeral4 = Rule
  { name = "number (21..29)"
  , pattern =
      [oneOf [20], regex "-i-", numberBetween 1 10]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData { TNumeral.value = v1 } : _ : Token Numeral NumeralData { TNumeral.value = v2 } : _) ->
        double $ v1 + v2
      _ -> Nothing
  }

ruleNumeral7 :: Rule
ruleNumeral7 = Rule
  { name = "number (31..39 41..49 51..59 61..69 71..79 81..89 91..99)"
  , pattern =
      [oneOf [70, 60, 50, 40, 90, 30, 80], regex "-", numberBetween 1 10]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData { TNumeral.value = v1 } : _ : Token Numeral NumeralData { TNumeral.value = v2 } : _) ->
        double $ v1 + v2
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
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData { TNumeral.value = v1 } : _ : Token Numeral NumeralData { TNumeral.value = v2 } : _) ->
        double $ 100 * v1 + v2
      _ -> Nothing
  }

ruleNumeralDotNumeral :: Rule
ruleNumeralDotNumeral = Rule
  { name = "number dot number"
  , pattern = [dimension Numeral, regex "coma", Predicate $ not . hasGrain]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData { TNumeral.value = v1 } : _ : Token Numeral NumeralData { TNumeral.value = v2 } : _) ->
        double $ v1 + decimalsToDouble v2
      _ -> Nothing
  }

ruleBelowTenWithTwoDigits :: Rule
ruleBelowTenWithTwoDigits = Rule
  {
    name = "integer (0-9) with two digits"
  , pattern =
      [
        regex "zero|0"
      , numberBetween 1 10
      ]
  , prod = \case
      (
        _:
        Token Numeral NumeralData { TNumeral.value = v }:
        _
        ) -> double v
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleBelowTenWithTwoDigits
  , ruleNumeral
  , ruleNumeral2
  -- , ruleNumeral3
  , ruleNumeral4
  , ruleNumeral7  -- afegida per 30..90
  , ruleNumeral5
  , ruleNumeral6
  , ruleNumeralDotNumeral
  , ruleNumerals
  , ruleNumeralsPrefixWithNegativeOrMinus
  , ruleNumeralsSuffixesKMG
  ]
