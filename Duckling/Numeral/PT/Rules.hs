-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.PT.Rules
  ( rules ) where

import Data.Maybe
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
    [ regex "-|menos"
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
        v <- parseInt match
        integer $ toInteger v
      _ -> Nothing
  }

ruleDecimalWithThousandsSeparator :: Rule
ruleDecimalWithThousandsSeparator = Rule
  { name = "decimal with thousands separator"
  , pattern =
    [ regex "(\\d+(\\.\\d\\d\\d)+,\\d+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):
       _) -> let dot = Text.singleton '.'
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
    [ regex "(vinte|trinta|quarenta|cincoenta|cinq(\x00fc)enta|cinquenta|sessenta|setenta|oitenta|noventa)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "vinte" -> integer 20
        "trinta" -> integer 30
        "quarenta" -> integer 40
        "cinq\252enta" -> integer 50
        "cincoenta" -> integer 50
        "cinquenta" -> integer 50
        "sessenta" -> integer 60
        "setenta" -> integer 70
        "oitenta" -> integer 80
        "noventa" -> integer 90
        _ -> Nothing
      _ -> Nothing
  }

ruleNumeral :: Rule
ruleNumeral = Rule
  { name = "number (0..15)"
  , pattern =
    [ regex "(zero|uma?|d(oi|ua)s|tr(\x00ea|e)s|quatro|cinco|seis|sete|oito|nove|dez|onze|doze|treze|(ca|qua)torze|quinze)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "zero" -> integer 0
        "uma" -> integer 1
        "um" -> integer 1
        "dois" -> integer 2
        "duas" -> integer 2
        "tr\x00eas" -> integer 3
        "tres" -> integer 3
        "quatro" -> integer 4
        "cinco" -> integer 5
        "seis" -> integer 6
        "sete" -> integer 7
        "oito" -> integer 8
        "nove" -> integer 9
        "dez" -> integer 10
        "onze" -> integer 11
        "doze" -> integer 12
        "treze" -> integer 13
        "catorze" -> integer 14
        "quatorze" -> integer 14
        "quinze" -> integer 15
        _ -> Nothing
      _ -> Nothing
  }

ruleNumeral5 :: Rule
ruleNumeral5 = Rule
  { name = "number (16..19)"
  , pattern =
    [ regex "(dez[ea]sseis|dez[ea]ssete|dezoito|dez[ea]nove)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "dezesseis" -> integer 16
        "dezasseis" -> integer 16
        "dezessete" -> integer 17
        "dezassete" -> integer 17
        "dezoito" -> integer 18
        "dezenove" -> integer 19
        "dezanove" -> integer 19
        _ -> Nothing
      _ -> Nothing
  }

ruleNumeral3 :: Rule
ruleNumeral3 = Rule
  { name = "number (16..19)"
  , pattern =
    [ numberWith TNumeral.value (== 10)
    , regex "e"
    , numberBetween 6 10
    ]
  , prod = \tokens -> case tokens of
      (_:Token Numeral (NumeralData {TNumeral.value = v}):_) -> double $ 10 + v
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

ruleNumeral6 :: Rule
ruleNumeral6 = Rule
  { name = "number 100..1000 "
  , pattern =
    [
      regex "(cem|cento|duzentos|trezentos|quatrocentos|quinhentos|seiscentos|setecentos|oitocentos|novecentos|mil)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "cento" -> integer 100
        "cem" -> integer 100
        "duzentos" -> integer 200
        "trezentos" -> integer 300
        "quatrocentos" -> integer 400
        "quinhentos" -> integer 500
        "seiscentos" -> integer 600
        "setecentos" -> integer 700
        "oitocentos" -> integer 800
        "novecentos" -> integer 900
        "mil" -> integer 1000
        _ -> Nothing
      _ -> Nothing
  }

ruleNumeral4 :: Rule
ruleNumeral4 = Rule
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

ruleDozen :: Rule
ruleDozen = Rule
  { name = "dozen"
  , pattern =
    [ regex "d(\x00fa|u)zias?"
    ]
  , prod = \_ -> integer 12 >>= withGrain 1 >>= withMultipliable
  }

ruleNumeralDozen :: Rule
ruleNumeralDozen = Rule
  { name = "number dozen"
  , pattern =
    [ numberBetween 1 11
    , dimension Numeral
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = v1}):
       Token Numeral (NumeralData {TNumeral.value = v2, TNumeral.grain = Just g}):
       _) -> double (v1 * v2) >>= withGrain g
      _ -> Nothing
  }

ruleNumerals :: Rule
ruleNumerals = Rule
  { name = "numbers (100..999)"
  , pattern =
    [ numberBetween 100 1000
    , regex "e"
    , numberBetween 0 100
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = v1}):
       _:
       Token Numeral (NumeralData {TNumeral.value = v2}):
       _) -> double $ v1 + v2
      _ -> Nothing
  }

ruleNumeralDotNumeral :: Rule
ruleNumeralDotNumeral = Rule
  { name = "number dot number"
  , pattern =
    [ dimension Numeral
    , regex "ponto"
    , numberWith TNumeral.grain isNothing
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral nd1:_:Token Numeral nd2:_) ->
        double $ TNumeral.value nd1 + decimalsToDouble (TNumeral.value nd2)
      _ -> Nothing
  }

ruleIntegerWithThousandsSeparator :: Rule
ruleIntegerWithThousandsSeparator = Rule
  { name = "integer with thousands separator ."
  , pattern =
    [ regex "(\\d{1,3}(\\.\\d\\d\\d){1,5})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):
       _) -> let fmt = Text.replace (Text.singleton '.') Text.empty match
        in parseDouble fmt >>= double
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleDecimalNumeral
  , ruleDecimalWithThousandsSeparator
  , ruleDozen
  , ruleIntegerNumeric
  , ruleIntegerWithThousandsSeparator
  , ruleNumeral
  , ruleNumeral2
  , ruleNumeral3
  , ruleNumeral4
  , ruleNumeral5
  , ruleNumeral6
  , ruleNumeralDotNumeral
  , ruleNumeralDozen
  , ruleNumerals
  , ruleNumeralsPrefixWithNegativeOrMinus
  , ruleNumeralsSuffixesKMG
  ]
