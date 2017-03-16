-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.IT.Rules
  ( rules ) where

import qualified Data.Text as Text
import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers
import Duckling.Numeral.Types (NumeralData (..))
import qualified Duckling.Numeral.Types as TNumeral
import Duckling.Regex.Types
import Duckling.Types

ruleNumeralsPrefixWithNegativeOrMinus :: Rule
ruleNumeralsPrefixWithNegativeOrMinus = Rule
  { name = "numbers prefix with -, negative or minus"
  , pattern =
    [ regex "-|meno|negativo"
    , dimension Numeral
    ]
  , prod = \tokens -> case tokens of
      (_:Token Numeral nd:_) -> double (TNumeral.value nd * (-1))
      _ -> Nothing
  }

ruleIntegerNumeric :: Rule
ruleIntegerNumeric = Rule
  { name = "integer (numeric)"
  , pattern =
    [ regex "(\\d{1,18})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        v <- toInteger <$> parseInt match
        integer v
      _ -> Nothing
  }

ruleDecimalWithThousandsSeparator :: Rule
ruleDecimalWithThousandsSeparator = Rule
  { name = "decimal with thousands separator"
  , pattern =
    [ regex "(\\d+(\\.\\d\\d\\d)+,\\d+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        let dot = Text.singleton '.'
            comma = Text.singleton ','
            fmt = Text.replace comma dot $ Text.replace dot Text.empty match
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
      (Token RegexMatch (GroupMatch (match:_)):
       _) -> parseDecimal False match
      _ -> Nothing
  }

ruleNumeral2 :: Rule
ruleNumeral2 = Rule
  { name = "number (20..90)"
  , pattern =
    [ regex "(venti|trenta|quaranta|cinquanta|sessanta|settanta|ottanta|novanta)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "venti" -> integer 20
        "trenta" -> integer 30
        "quaranta" -> integer 40
        "cinquanta" -> integer 50
        "sessanta" -> integer 60
        "settanta" -> integer 70
        "ottanta" -> integer 80
        "novanta" -> integer 90
        _ -> Nothing
      _ -> Nothing
  }

ruleNumeral :: Rule
ruleNumeral = Rule
  { name = "number (0..19)"
  , pattern =
    [ regex "(zero|nulla|niente|uno|due|tredici|tre|quattro|cinque|sei|sette|otto|nove|dieci|undici|dodici|quattordici|quindici|sedici|diciassette|diciotto|diciannove|un)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "zero" -> integer 0
        "niente" -> integer 0
        "nulla" -> integer 0
        "un" -> integer 1
        "uno" -> integer 1
        "due" -> integer 2
        "tre" -> integer 3
        "quattro" -> integer 4
        "cinque" -> integer 5
        "sei" -> integer 6
        "sette" -> integer 7
        "otto" -> integer 8
        "nove" -> integer 9
        "dieci" -> integer 10
        "undici" -> integer 11
        "dodici" -> integer 12
        "tredici" -> integer 13
        "quattordici" -> integer 14
        "quindici" -> integer 15
        "sedici" -> integer 16
        "diciassette" -> integer 17
        "diciotto" -> integer 18
        "diciannove" -> integer 19
        _ -> Nothing
      _ -> Nothing
  }

ruleNumeral5 :: Rule
ruleNumeral5 = Rule
  { name = "number 100..1000 "
  , pattern =
    [ regex "(due|tre|quattro|cinque|sei|sette|otto|nove)?cento|mil(a|le)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "cento" -> integer 100
        "duecento" -> integer 200
        "trecento" -> integer 300
        "quattrocento" -> integer 400
        "cinquecento" -> integer 500
        "seicento" -> integer 600
        "settecento" -> integer 700
        "ottocento" -> integer 800
        "novecento" -> integer 900
        " mila" -> integer 1000
        "mille" -> integer 1000
        _ -> Nothing
      _ -> Nothing
  }

ruleNumeral3 :: Rule
ruleNumeral3 = Rule
  { name = "number (21..29 31..39 41..49 51..59 61..69 71..79 81..89 91..99)"
  , pattern =
    [ oneOf [70, 20, 60, 50, 40, 90, 30, 80]
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

ruleNumeralsSuffixesKMG :: Rule
ruleNumeralsSuffixesKMG = Rule
  { name = "numbers suffixes (K, M, G)"
  , pattern =
    [ dimension Numeral
    , regex "([kmg])(?=[\\W\\$\x20ac]|$)"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = v}):
       Token RegexMatch (GroupMatch (match:_)):
       _) -> case Text.toLower match of
         "k" -> double $ v * 1e3
         "m" -> double $ v * 1e6
         "g" -> double $ v * 1e9
         _   -> Nothing
      _ -> Nothing
  }

ruleNumeral4 :: Rule
ruleNumeral4 = Rule
  { name = "number (21..29 31..39 41..49 51..59 61..69 71..79 81..89 91..99)"
  , pattern =
    [ regex "((venti|trenta|quaranta|cinquanta|sessanta|settanta|ottanta|novanta)(due|tre|tr\x00e9|quattro|cinque|sei|sette|nove))|((vent|trent|quarant|cinquant|sessant|settant|ottant|novant)(uno|otto))"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "ventuno" -> integer 21
        "ventidue" -> integer 22
        "ventitre" -> integer 23
        "ventitr\x00e9" -> integer 23
        "ventiquattro" -> integer 24
        "venticinque" -> integer 25
        "ventisei" -> integer 26
        "ventisette" -> integer 27
        "ventotto" -> integer 28
        "ventinove" -> integer 29
        "trentuno" -> integer 31
        "trentadue" -> integer 32
        "trentatre" -> integer 33
        "trentatr\x00e9" -> integer 33
        "trentaquattro" -> integer 34
        "trentacinque" -> integer 35
        "trentasei" -> integer 36
        "trentasette" -> integer 37
        "trentotto" -> integer 38
        "trentanove" -> integer 39
        "quarantuno" -> integer 41
        "quarantadue" -> integer 42
        "quarantatre" -> integer 43
        "quarantatr\x00e9" -> integer 43
        "quarantaquattro" -> integer 44
        "quarantacinque" -> integer 45
        "quarantasei" -> integer 46
        "quarantasette" -> integer 47
        "quarantotto" -> integer 48
        "quarantanove" -> integer 49
        "cinquantuno" -> integer 51
        "cinquantadue" -> integer 52
        "cinquantatre" -> integer 53
        "cinquantatr\x00e9" -> integer 53
        "cinquantaquattro" -> integer 54
        "cinquantacinque" -> integer 55
        "cinquantasei" -> integer 56
        "cinquantasette" -> integer 57
        "cinquantotto" -> integer 58
        "cinquantanove" -> integer 59
        "sessantuno" -> integer 61
        "sessantadue" -> integer 62
        "sessantatr\x00e9" -> integer 63
        "sessantatre" -> integer 63
        "sessantaquattro" -> integer 64
        "sessantacinque" -> integer 65
        "sessantasei" -> integer 66
        "sessantasette" -> integer 67
        "sessantotto" -> integer 68
        "sessantanove" -> integer 69
        "settantuno" -> integer 71
        "settantadue" -> integer 72
        "settantatr\x00e9" -> integer 73
        "settantatre" -> integer 73
        "settantaquattro" -> integer 74
        "settantacinque" -> integer 75
        "settantasei" -> integer 76
        "settantasette" -> integer 77
        "settantotto" -> integer 78
        "settantanove" -> integer 79
        "ottantuno" -> integer 81
        "ottantadue" -> integer 82
        "ottantatr\x00e9" -> integer 83
        "ottantatre" -> integer 83
        "ottantaquattro" -> integer 84
        "ottantacinque" -> integer 85
        "ottantasei" -> integer 86
        "ottantasette" -> integer 87
        "ottantotto" -> integer 88
        "ottantanove" -> integer 89
        "novantuno" -> integer 91
        "novantadue" -> integer 92
        "novantatre" -> integer 93
        "novantatr\x00e9" -> integer 93
        "novantaquattro" -> integer 94
        "novantacinque" -> integer 95
        "novantasei" -> integer 96
        "novantasette" -> integer 97
        "novantotto" -> integer 98
        "novantanove" -> integer 99
        _ -> Nothing
      _ -> Nothing
  }

ruleNumerals :: Rule
ruleNumerals = Rule
  { name = "numbers 200..999"
  , pattern =
    [ numberBetween 2 10
    , numberWith TNumeral.value (== 100)
    , numberBetween 0 100
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = v1}):
       Token Numeral (NumeralData {TNumeral.value = v2}):
       Token Numeral (NumeralData {TNumeral.value = v3}):
       _) -> double $ v1 * v2 + v3
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
        parseDouble (Text.replace (Text.singleton '.') Text.empty match) >>= double
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleDecimalNumeral
  , ruleDecimalWithThousandsSeparator
  , ruleIntegerNumeric
  , ruleIntegerWithThousandsSeparator
  , ruleNumeral
  , ruleNumeral2
  , ruleNumeral3
  , ruleNumeral4
  , ruleNumeral5
  , ruleNumerals
  , ruleNumeralsPrefixWithNegativeOrMinus
  , ruleNumeralsSuffixesKMG
  ]
