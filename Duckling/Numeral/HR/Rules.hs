-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.HR.Rules
  ( rules ) where

import Data.Maybe
import Data.String
import Data.Text (Text)
import Prelude
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Regex.Types
import Duckling.Types
import qualified Duckling.Numeral.Types as TNumeral

ruleNumbersPrefixWithNegativeOrMinus :: Rule
ruleNumbersPrefixWithNegativeOrMinus = Rule
  { name = "numbers prefix with -, negative or minus"
  , pattern =
    [ regex "-|minus|negativ"
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

ruleFew :: Rule
ruleFew = Rule
  { name = "few"
  , pattern =
    [ regex "nekoliko"
    ]
  , prod = \_ -> integer 3
  }

ruleTen :: Rule
ruleTen = Rule
  { name = "ten"
  , pattern =
    [ regex "deset|cener"
    ]
  , prod = \_ -> integer 10 >>= withGrain 1
  }

ruleDecimalWithThousandsSeparator :: Rule
ruleDecimalWithThousandsSeparator = Rule
  { name = "decimal with thousands separator"
  , pattern =
    [ regex "(\\d+(\\.\\d\\d\\d)+\\,\\d+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        let dot = Text.singleton '.'
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

ruleInteger3 :: Rule
ruleInteger3 = Rule
  { name = "integer (100..900)"
  , pattern =
    [ regex "(sto|dvjest(o|a)|tristo|(c|\x010d)etiristo|petsto|(\x0161|s)esto|sedamsto|osamsto|devetsto)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "sto" -> integer 100
        "dvjesta" -> integer 200
        "dvjesto" -> integer 200
        "tristo" -> integer 300
        "cetiristo" -> integer 400
        "\269etiristo" -> integer 400
        "petsto" -> integer 500
        "\353esto" -> integer 600
        "sesto" -> integer 600
        "sedamsto" -> integer 700
        "osamsto" -> integer 800
        "devetsto" -> integer 900
        _ -> Nothing
      _ -> Nothing
  }

ruleSingle :: Rule
ruleSingle = Rule
  { name = "single"
  , pattern =
    [ regex "sam"
    ]
  , prod = \_ -> integer 1 >>= withGrain 1
  }

rulePowersOfTen :: Rule
rulePowersOfTen = Rule
  { name = "powers of tens"
  , pattern =
    [ regex "(stotin(u|a|e)|tisu(c|\x0107)(a|u|e)|milij(u|o)na?)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "stotinu" -> double 1e2 >>= withGrain 2 >>= withMultipliable
        "stotina" -> double 1e2 >>= withGrain 2 >>= withMultipliable
        "stotine" -> double 1e2 >>= withGrain 2 >>= withMultipliable
        "tisuca" -> double 1e3 >>= withGrain 3 >>= withMultipliable
        "tisucu" -> double 1e3 >>= withGrain 3 >>= withMultipliable
        "tisuce" -> double 1e3 >>= withGrain 3 >>= withMultipliable
        "tisu\263a" -> double 1e3 >>= withGrain 3 >>= withMultipliable
        "tisu\263u" -> double 1e3 >>= withGrain 3 >>= withMultipliable
        "tisu\263e" -> double 1e3 >>= withGrain 3 >>= withMultipliable
        "milijun" -> double 1e6 >>= withGrain 6 >>= withMultipliable
        "milijuna" -> double 1e6 >>= withGrain 6 >>= withMultipliable
        "milijon" -> double 1e6 >>= withGrain 6 >>= withMultipliable
        "milijona" -> double 1e6 >>= withGrain 6 >>= withMultipliable
        _ -> Nothing
      _ -> Nothing
  }

ruleNumbersI :: Rule
ruleNumbersI = Rule
  { name = "numbers i"
  , pattern =
    [ oneOf [70, 20, 60, 50, 40, 90, 30, 80]
    , regex "i"
    , numberBetween 1 10
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = v1}):
       _:
       Token Numeral (NumeralData {TNumeral.value = v2}):
       _) -> double $ v1 + v2
      _ -> Nothing
  }

ruleSum :: Rule
ruleSum = Rule
  { name = "intersect"
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

ruleNumbersSuffixesKMG :: Rule
ruleNumbersSuffixesKMG = Rule
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

ruleAPair :: Rule
ruleAPair = Rule
  { name = "a pair"
  , pattern =
    [ regex "par"
    ]
  , prod = \_ -> integer 2 >>= withGrain 1
  }

ruleDozen :: Rule
ruleDozen = Rule
  { name = "dozen"
  , pattern =
    [ regex "tucet?"
    ]
  , prod = \_ -> integer 12 >>= withGrain 1
  }

ruleInteger :: Rule
ruleInteger = Rule
  { name = "integer (0..19)"
  , pattern =
    [ regex "(ni(s|\x0161)ta|ni(s|\x0161)tica|nula|jedanaest|dvanaest|trinaest|jeda?n(a|u|o(ga?)?)?|dv(i?je)?(a|o)?(ma)?|tri(ma)?|(\x010d|c)etiri|(\x010d|c)etrnaest|petnaest|pet|(s|\x0161)esnaest|(\x0161|s)est|sedamnaest|sedam|osamnaest|osam|devetnaest|devet)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "ni\353ta" -> integer 0
        "ni\353tica" -> integer 0
        "nistica" -> integer 0
        "nista" -> integer 0
        "nula" -> integer 0
        "jednoga" -> integer 1
        "jedna" -> integer 1
        "jednog" -> integer 1
        "jednu" -> integer 1
        "jedan" -> integer 1
        "dvoma" -> integer 2
        "dvije" -> integer 2
        "dvje" -> integer 2
        "dva" -> integer 2
        "dvama" -> integer 2
        "trima" -> integer 3
        "tri" -> integer 3
        "\269etiri" -> integer 4
        "cetiri" -> integer 4
        "pet" -> integer 5
        "\353est" -> integer 6
        "sedam" -> integer 7
        "osam" -> integer 8
        "devet" -> integer 9
        "jedanaest" -> integer 11
        "dvanaest" -> integer 12
        "trinaest" -> integer 13
        "cetrnaest" -> integer 14
        "\269etrnaest" -> integer 14
        "petnaest" -> integer 15
        "\353esnaest" -> integer 16
        "sesnaest" -> integer 16
        "sedamnaest" -> integer 17
        "osamnaest" -> integer 18
        "devetnaest" -> integer 19
        _ -> Nothing
      _ -> Nothing
  }

ruleInteger4 :: Rule
ruleInteger4 = Rule
  { name = "integer 21..99"
  , pattern =
    [ oneOf [70, 20, 60, 50, 40, 90, 30, 80]
    , numberBetween 1 10
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = v1}):
       Token Numeral (NumeralData {TNumeral.value = v2}):
       _) -> double $ v1 + v2
      _ -> Nothing
  }

ruleInteger2 :: Rule
ruleInteger2 = Rule
  { name = "integer (20..90)"
  , pattern =
    [ regex "(dvadeset|trideset|(c|\x010d)etrdeset|pedeset|(\x0161|s)esdeset|sedamdeset|osamdeset|devedeset)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "dvadeset" -> integer 20
        "trideset" -> integer 30
        "cetrdeset" -> integer 40
        "\269etrdeset" -> integer 40
        "pedeset" -> integer 50
        "sesdeset" -> integer 60
        "\353esdeset" -> integer 60
        "sedamdeset" -> integer 70
        "osamdeset" -> integer 80
        "devedeset" -> integer 90
        _ -> Nothing
      _ -> Nothing
  }

ruleNumbers :: Rule
ruleNumbers = Rule
  { name = "numbers 100..999"
  , pattern =
    [ numberBetween 1 10
    , numberWith TNumeral.value (== 100)
    , numberBetween 0 100
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = v1}):
       _:
       Token Numeral (NumeralData {TNumeral.value = v2}):
       _) -> double $ 100 * v1 + v2
      _ -> Nothing
  }

ruleNumberDotNumber :: Rule
ruleNumberDotNumber = Rule
  { name = "number dot number"
  , pattern =
    [ dimension Numeral
    , regex "cijela|to(c|\x010d)ka|zarez"
    , numberWith TNumeral.grain isNothing
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = v1}):
       _:
       Token Numeral (NumeralData {TNumeral.value = v2}):
       _) -> double $ v1 + decimalsToDouble v2
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
  [ ruleAPair
  , ruleDecimalNumber
  , ruleDecimalWithThousandsSeparator
  , ruleDozen
  , ruleFew
  , ruleInteger
  , ruleInteger2
  , ruleInteger3
  , ruleInteger4 , ruleIntegerNumeric
  , ruleIntegerWithThousandsSeparator
  , ruleMultiply
  , ruleNumberDotNumber
  , ruleNumbers
  , ruleNumbersI
  , ruleNumbersPrefixWithNegativeOrMinus
  , ruleNumbersSuffixesKMG
  , rulePowersOfTen
  , ruleSingle
  , ruleSum
  , ruleTen
  ]
