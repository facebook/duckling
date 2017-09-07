-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.GA.Rules
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
    [ regex "-|m(í|i)neas(\\sa)?\\s?"
    , dimension Numeral
    ]
  , prod = \tokens -> case tokens of
      (_:
       Token Numeral (NumeralData {TNumeral.value = v}):
       _) -> double $ v * (-1)
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

ruleNumerals2 :: Rule
ruleNumerals2 = Rule
  { name = "numbers, 1-10"
  , pattern =
    [ regex "(aon|dh(á|a)|tr(í|i)|ceithre|c(ú|u)ig|seacht|s(é|e)|ocht|naoi|deich)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "aon" -> integer 1
        "dha" -> integer 2
        "dhá" -> integer 2
        "trí" -> integer 3
        "tri" -> integer 3
        "ceithre" -> integer 4
        "cuig" -> integer 5
        "cúig" -> integer 5
        "sé" -> integer 6
        "se" -> integer 6
        "seacht" -> integer 7
        "ocht" -> integer 8
        "naoi" -> integer 9
        "deich" -> integer 10
        _ -> Nothing
      _ -> Nothing
  }

ruleDecimalWithThousandsSeparator :: Rule
ruleDecimalWithThousandsSeparator = Rule
  { name = "decimal with thousands separator"
  , pattern =
    [ regex "(\\d+(,\\d\\d\\d)+\\.\\d+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        parseDouble (Text.replace (Text.singleton ',') Text.empty match) >>= double
      _ -> Nothing
  }

ruleDecimalNumeral :: Rule
ruleDecimalNumeral = Rule
  { name = "decimal number"
  , pattern =
    [ regex "(\\d*\\.\\d+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> parseDecimal True match
      _ -> Nothing
  }

ruleDag :: Rule
ruleDag = Rule
  { name = "déag"
  , pattern =
    [ regex "d(é|e)ag"
    ]
  , prod = \_ -> integer 10
  }

ruleNumeralsSuffixesKMG :: Rule
ruleNumeralsSuffixesKMG = Rule
  { name = "numbers suffixes (K, M, G)"
  , pattern =
    [ dimension Numeral
    , regex "([kmg])(?=[\\W\\$€]|$)"
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

ruleOldVigesimalNumeralsS :: Rule
ruleOldVigesimalNumeralsS = Rule
  { name = "old vigesimal numbers, 20s"
  , pattern =
    [ regex "is (dh?(á|a) fhichead|tr(í|i) fichid|ceithre fichid)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "dá fhichead" -> integer 40
        "da fhichead" -> integer 40
        "dhá fhichead" -> integer 40
        "dha fhichead" -> integer 40
        "trí fichid" -> integer 60
        "tri fichid" -> integer 60
        "ceithre fichid" -> integer 80
        _ -> Nothing
      _ -> Nothing
  }

ruleOldVigesimalNumeralsS2 :: Rule
ruleOldVigesimalNumeralsS2 = Rule
  { name = "old vigesimal numbers, 20s + 10"
  , pattern =
    [ regex "d(é|e)ag is (fiche|dh?(á|a) fhichead|tr(í|i) fichid|ceithre fichid)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "fiche" -> integer 30
        "dá fhichead" -> integer 50
        "da fhichead" -> integer 50
        "dhá fhichead" -> integer 50
        "dha fhichead" -> integer 50
        "trí fichid" -> integer 70
        "tri fichid" -> integer 70
        "ceithre fichid" -> integer 90
        _ -> Nothing
      _ -> Nothing
  }

ruleAmhin :: Rule
ruleAmhin = Rule
  { name = "amháin"
  , pattern =
    [ regex "amh(á|a)in"
    ]
  , prod = \_ -> integer 1
  }

ruleNumerals :: Rule
ruleNumerals = Rule
  { name = "numbers, 20-90"
  , pattern =
    [ regex "(fiche|tr(í|i)ocha|daichead|caoga|seasca|seacht(ó|o)|ocht(ó|o)|n(ó|o)cha)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "fiche" -> integer 20
        "triocha" -> integer 30
        "tríocha" -> integer 30
        "daichead" -> integer 40
        "caoga" -> integer 50
        "seasca" -> integer 60
        "seachto" -> integer 70
        "seachtó" -> integer 70
        "ochto" -> integer 80
        "ochtó" -> integer 80
        "nócha" -> integer 90
        "nocha" -> integer 90
        _ -> Nothing
      _ -> Nothing
  }

ruleIntegerWithThousandsSeparator :: Rule
ruleIntegerWithThousandsSeparator = Rule
  { name = "integer with thousands separator ,"
  , pattern =
    [ regex "(\\d{1,3}(,\\d\\d\\d){1,5})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        parseDouble (Text.replace (Text.singleton ',') Text.empty match) >>= double
      _ -> Nothing
  }

ruleCountNumerals :: Rule
ruleCountNumerals = Rule
  { name = "count numbers"
  , pattern =
    [ regex "a (n(á|a)id|haon|d(ó|o)|tr(í|i)|ceathair|c(ú|u)ig|s(é|e)|seacht|hocht|naoi|deich)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "naid" -> integer 0
        "náid" -> integer 0
        "haon" -> integer 1
        "dó" -> integer 2
        "do" -> integer 2
        "trí" -> integer 3
        "tri" -> integer 3
        "ceathair" -> integer 4
        "cuig" -> integer 5
        "cúig" -> integer 5
        "sé" -> integer 6
        "se" -> integer 6
        "seacht" -> integer 7
        "hocht" -> integer 8
        "naoi" -> integer 9
        "deich" -> integer 10
        _ -> Nothing
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleAmhin
  , ruleCountNumerals
  , ruleDag
  , ruleDecimalNumeral
  , ruleDecimalWithThousandsSeparator
  , ruleIntegerNumeric
  , ruleIntegerWithThousandsSeparator
  , ruleNumerals
  , ruleNumerals2
  , ruleNumeralsPrefixWithNegativeOrMinus
  , ruleNumeralsSuffixesKMG
  , ruleOldVigesimalNumeralsS
  , ruleOldVigesimalNumeralsS2
  ]
