-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Numeral.BG.Rules
  ( rules
  ) where

import Data.HashMap.Strict (HashMap)
import Data.Maybe
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

zeroNineteenMap :: HashMap Text Integer
zeroNineteenMap = HashMap.fromList
  [ ( "нула", 0 )
  , ( "един", 1 )
  , ( "една", 1 )
  , ( "едно", 1 )
  , ( "два", 2 )
  , ( "две", 2 )
  , ( "три", 3 )
  , ( "четири", 4 )
  , ( "пет", 5)
  , ( "шест", 6)
  , ( "седем", 7)
  , ( "осем", 8)
  , ( "девет", 9)
  , ( "десет", 10)
  , ( "единадесет", 11 )
  , ( "единайсет", 11 )
  , ( "дванадесет", 12 )
  , ( "дванайсет", 12 )
  , ( "тринадесет", 13 )
  , ( "тринайсет", 13 )
  , ( "четиринадесет", 14)
  , ( "четиринайсет", 14)
  , ( "петнадесет", 15)
  , ( "петнайсет", 15)
  , ( "шестнадесет", 16)
  , ( "шестнайсет", 16)
  , ( "седемнадесет", 17)
  , ( "седемнайсет", 17)
  , ( "осемнадесет", 18)
  , ( "осемнайсет", 18)
  , ( "деветнадесет", 19)
  , ( "деветнайсет", 19)
  ]

ruleToNineteen :: Rule
ruleToNineteen = Rule
  { name = "number (0..19)"
  , pattern =
    [ regex "(нула|едина(де|й)сет|двана(де|й)сет|трина(де|й)сет|четирина(де|й)сет|петна(де|й)сет|шестна(де|й)сет|седемна(де|й)сет|осемна(де|й)сет|деветна(де|й)сет|един|една|едно|два|две|три|четири|пет|шест|седем|осем|девет|десет)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        let x = Text.toLower match in
        HashMap.lookup x zeroNineteenMap >>= integer
      _ -> Nothing
  }

ruleTens :: Rule
ruleTens = Rule
  { name = "integer (20..90)"
  , pattern =
    [ regex "((два|три|четири|пет|шест|седем|осем|девет)десет)"
    ]
  , prod = \tokens ->
      case tokens of
        (Token RegexMatch (GroupMatch (_:match:_)):_) -> do
          x <- HashMap.lookup (Text.toLower match) zeroNineteenMap
          integer $ x * 10
        _ -> Nothing
  }

rulePowersOfTen :: Rule
rulePowersOfTen = Rule
  { name = "powers of tens"
  , pattern =
    [ regex "(хиляд(а|и)|милион(а|и)?|милиард(а|и)?)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "хиляд"   -> double 1e3 >>= withGrain 3 >>= withMultipliable
        "милион"  -> double 1e6 >>= withGrain 6 >>= withMultipliable
        "милиард" -> double 1e9 >>= withGrain 9 >>= withMultipliable
        _         -> Nothing
      _ -> Nothing
  }

ruleCompositeTens :: Rule
ruleCompositeTens = Rule
  { name = "integer 21..99"
  , pattern =
    [ oneOf [20, 30..90]
    , regex "и"
    , numberBetween 1 10
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = tens}:
       _:
       Token Numeral NumeralData{TNumeral.value = units}:
       _) -> double $ tens + units
      _ -> Nothing
  }

ruleHundreds :: Rule
ruleHundreds = Rule
  { name = "integer (100..900)"
  , pattern =
    [ regex "(сто|двеста|триста|(четири|пет|шест|седем|осем|девет)стотин)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "сто"          -> integer 100
        "двеста"       -> integer 200
        "триста"       -> integer 300
        "четиристотин" -> integer 400
        "петстотин"    -> integer 500
        "шестстотин"   -> integer 600
        "седемстотин"  -> integer 700
        "осемстотин"   -> integer 800
        "деветстотин"  -> integer 900
        _              -> Nothing
      _ -> Nothing
  }

ruleCompositeHundreds :: Rule
ruleCompositeHundreds = Rule
  { name = "integer 101..999"
  , pattern =
    [ oneOf [200, 300..900]
    , numberBetween 1 100
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = hundreds}:
       Token Numeral NumeralData{TNumeral.value = tens}:
       _) -> double $ hundreds + tens
      _ -> Nothing
  }

ruleDotSpelledOut :: Rule
ruleDotSpelledOut = Rule
  { name = "one point 2"
  , pattern =
    [ dimension Numeral
    , regex "цяло и"
    , Predicate $ not . hasGrain
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral nd1:_:Token Numeral nd2:_) ->
        double $ TNumeral.value nd1 + decimalsToDouble (TNumeral.value nd2)
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
    , regex "((к|м|г)|(К|М|Г))(?=[\\W$€¢£]|$)"
    ]
  , prod = \tokens ->
      case tokens of
        (Token Numeral nd : Token RegexMatch (GroupMatch (match : _)):_) -> do
          x <- case Text.toLower match of
            "к" -> Just 1e3
            "К" -> Just 1e3
            "м" -> Just 1e6
            "М" -> Just 1e6
            "г" -> Just 1e9
            "Г" -> Just 1e9
            _ -> Nothing
          double $ TNumeral.value nd * x
        _ -> Nothing
  }

ruleNegative :: Rule
ruleNegative = Rule
  { name = "negative numbers"
  , pattern =
    [ regex "-|минус\\s?"
    , dimension Numeral
    ]
  , prod = \tokens -> case tokens of
      (_:Token Numeral nd:_) -> double (TNumeral.value nd * (-1))
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleToNineteen
  , ruleTens
  , rulePowersOfTen
  , ruleCompositeTens
  , ruleHundreds
  , ruleCompositeHundreds
  , ruleDotSpelledOut
  , ruleDecimals
  , ruleCommas
  , ruleSuffixes
  , ruleNegative
  ]
