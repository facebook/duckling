-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Number.PT.Rules
  ( rules ) where

import Data.Maybe
import qualified Data.Text as Text
import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Number.Helpers
import Duckling.Number.Types (NumberData (..))
import qualified Duckling.Number.Types as TNumber
import Duckling.Regex.Types
import Duckling.Types

ruleNumbersPrefixWithNegativeOrMinus :: Rule
ruleNumbersPrefixWithNegativeOrMinus = Rule
  { name = "numbers prefix with -, negative or minus"
  , pattern =
    [ regex "-|menos"
    , dimension DNumber
    ]
  , prod = \tokens -> case tokens of
      (_:Token DNumber nd:_) -> double (TNumber.value nd * (-1))
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

ruleDecimalNumber :: Rule
ruleDecimalNumber = Rule
  { name = "decimal number"
  , pattern =
    [ regex "(\\d*,\\d+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):
       _) -> parseDecimal False match
      _ -> Nothing
  }

ruleNumber2 :: Rule
ruleNumber2 = Rule
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

ruleNumber :: Rule
ruleNumber = Rule
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

ruleNumber5 :: Rule
ruleNumber5 = Rule
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

ruleNumber3 :: Rule
ruleNumber3 = Rule
  { name = "number (16..19)"
  , pattern =
    [ numberWith TNumber.value (== 10)
    , regex "e"
    , numberBetween 6 10
    ]
  , prod = \tokens -> case tokens of
      (_:Token DNumber (NumberData {TNumber.value = v}):_) -> double $ 10 + v
      _ -> Nothing
  }

ruleNumbersSuffixesKMG :: Rule
ruleNumbersSuffixesKMG = Rule
  { name = "numbers suffixes (K, M, G)"
  , pattern =
    [ dimension DNumber
    , regex "([kmg])(?=[\\W\\$\x20ac]|$)"
    ]
  , prod = \tokens -> case tokens of
      (Token DNumber (NumberData {TNumber.value = v}):
       Token RegexMatch (GroupMatch (match:_)):
       _) -> case Text.toLower match of
         "k" -> double $ v * 1e3
         "m" -> double $ v * 1e6
         "g" -> double $ v * 1e9
         _   -> Nothing
      _ -> Nothing
  }

ruleNumber6 :: Rule
ruleNumber6 = Rule
  { name = "number 100..1000 "
  , pattern =
    [ regex "(ce(m|to)|duzentos|trezentos|quatrocentos|quinhentos|seiscentos|setecentos|oitocentos|novecentos|mil)"
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

ruleNumber4 :: Rule
ruleNumber4 = Rule
  { name = "number (21..29 31..39 41..49 51..59 61..69 71..79 81..89 91..99)"
  , pattern =
    [ oneOf [70, 20, 60, 50, 40, 90, 30, 80]
    , regex "e"
    , numberBetween 1 10
    ]
  , prod = \tokens -> case tokens of
      (Token DNumber (NumberData {TNumber.value = v1}):
       _:
       Token DNumber (NumberData {TNumber.value = v2}):
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

ruleNumberDozen :: Rule
ruleNumberDozen = Rule
  { name = "number dozen"
  , pattern =
    [ numberBetween 1 11
    , dimension DNumber
    ]
  , prod = \tokens -> case tokens of
      (Token DNumber (NumberData {TNumber.value = v1}):
       Token DNumber (NumberData {TNumber.value = v2, TNumber.grain = Just g}):
       _) -> double (v1 * v2) >>= withGrain g
      _ -> Nothing
  }

ruleNumbers :: Rule
ruleNumbers = Rule
  { name = "numbers 200..999"
  , pattern =
    [ numberBetween 2 10
    , numberWith TNumber.value (== 100)
    , numberBetween 0 100
    ]
  , prod = \tokens -> case tokens of
      (Token DNumber (NumberData {TNumber.value = v1}):
       _:
       Token DNumber (NumberData {TNumber.value = v2}):
       _) -> double $ v1 * 100 + v2
      _ -> Nothing
  }

ruleNumberDotNumber :: Rule
ruleNumberDotNumber = Rule
  { name = "number dot number"
  , pattern =
    [ dimension DNumber
    , regex "ponto"
    , numberWith TNumber.grain isNothing
    ]
  , prod = \tokens -> case tokens of
      (Token DNumber nd1:_:Token DNumber nd2:_) ->
        double $ TNumber.value nd1 + decimalsToDouble (TNumber.value nd2)
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
  [ ruleDecimalNumber
  , ruleDecimalWithThousandsSeparator
  , ruleDozen
  , ruleIntegerNumeric
  , ruleIntegerWithThousandsSeparator
  , ruleNumber
  , ruleNumber2
  , ruleNumber3
  , ruleNumber4
  , ruleNumber5
  , ruleNumber6
  , ruleNumberDotNumber
  , ruleNumberDozen
  , ruleNumbers
  , ruleNumbersPrefixWithNegativeOrMinus
  , ruleNumbersSuffixesKMG
  ]
