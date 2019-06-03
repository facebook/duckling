-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.GA.Rules
  ( rules
  ) where

import Data.HashMap.Strict (HashMap)
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

ruleNumeralsPrefixWithNegativeOrMinus :: Rule
ruleNumeralsPrefixWithNegativeOrMinus = Rule
  { name = "numbers prefix with -, negative or minus"
  , pattern =
    [ regex "-|m(í|i)neas(\\sa)?"
    , Predicate isPositive
    ]
  , prod = \tokens -> case tokens of
      (_:
       Token Numeral NumeralData{TNumeral.value = v}:
       _) -> double $ v * (-1)
      _ -> Nothing
  }

oneToTenMap :: HashMap Text Integer
oneToTenMap = HashMap.fromList
  [ ("aon", 1)
  , ("dha", 2)
  , ("dhá", 2)
  , ("trí", 3)
  , ("tri", 3)
  , ("ceithre", 4)
  , ("cuig", 5)
  , ("cúig", 5)
  , ("sé", 6)
  , ("se", 6)
  , ("seacht", 7)
  , ("ocht", 8)
  , ("naoi", 9)
  , ("deich", 10)
  ]

ruleNumerals2 :: Rule
ruleNumerals2 = Rule
  { name = "numbers, 1-10"
  , pattern =
    [ regex "(aon|dh(á|a)|tr(í|i)|ceithre|c(ú|u)ig|seacht|s(é|e)|ocht|naoi|deich)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) oneToTenMap >>= integer
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
        parseDouble (Text.replace "," Text.empty match) >>= double
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
      (Token Numeral NumeralData{TNumeral.value = v}:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> case Text.toLower match of
         "k" -> double $ v * 1e3
         "m" -> double $ v * 1e6
         "g" -> double $ v * 1e9
         _   -> Nothing
      _ -> Nothing
  }

oldVigNumeralsSMap :: HashMap Text Integer
oldVigNumeralsSMap = HashMap.fromList
  [ ("dá fhichead", 40)
  , ("da fhichead", 40)
  , ("dhá fhichead", 40)
  , ("dha fhichead", 40)
  , ("trí fichid", 60)
  , ("tri fichid", 60)
  , ("ceithre fichid", 80)
  ]

ruleOldVigesimalNumeralsS :: Rule
ruleOldVigesimalNumeralsS = Rule
  { name = "old vigesimal numbers, 20s"
  , pattern =
    [ regex "(d[ée]ag )?is (dh?(á|a) fhichead|tr(í|i) fichid|ceithre fichid)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (ten:match:_)):_) -> do
        x <- HashMap.lookup (Text.toLower match) oldVigNumeralsSMap
        integer $ if Text.null ten then x else x + 10
      _ -> Nothing
  }

ruleOldVigesimalFiche :: Rule
ruleOldVigesimalFiche = Rule
  { name = "old vigesimal 20 + 10"
  , pattern =
    [ regex "d[ée]ag is fiche"
    ]
  , prod = const $ integer 30
  }

ruleAmhin :: Rule
ruleAmhin = Rule
  { name = "amháin"
  , pattern =
    [ regex "amh(á|a)in"
    ]
  , prod = \_ -> integer 1
  }

twentyToNinetyMap :: HashMap Text Integer
twentyToNinetyMap = HashMap.fromList
 [ ("fiche", 20)
 , ("triocha", 30)
 , ("tríocha", 30)
 , ("daichead", 40)
 , ("caoga", 50)
 , ("seasca", 60)
 , ("seachto", 70)
 , ("seachtó", 70)
 , ("ochto", 80)
 , ("ochtó", 80)
 , ("nócha", 90)
 , ("nocha", 90)
 ]

ruleNumerals :: Rule
ruleNumerals = Rule
  { name = "numbers, 20-90"
  , pattern =
    [ regex "(fiche|tr(í|i)ocha|daichead|caoga|seasca|seacht(ó|o)|ocht(ó|o)|n(ó|o)cha)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) twentyToNinetyMap >>= integer
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
        parseDouble (Text.replace "," Text.empty match) >>= double
      _ -> Nothing
  }

countNumeralsMap :: HashMap Text Integer
countNumeralsMap = HashMap.fromList
  [ ("naid", 0)
  , ("náid", 0)
  , ("haon", 1)
  , ("dó", 2)
  , ("do", 2)
  , ("trí", 3)
  , ("tri", 3)
  , ("ceathair", 4)
  , ("cuig", 5)
  , ("cúig", 5)
  , ("sé", 6)
  , ("se", 6)
  , ("seacht", 7)
  , ("hocht", 8)
  , ("naoi", 9)
  , ("deich", 10)
  ]

ruleCountNumerals :: Rule
ruleCountNumerals = Rule
  { name = "count numbers"
  , pattern =
    [ regex "a (n(á|a)id|haon|d(ó|o)|tr(í|i)|ceathair|c(ú|u)ig|s(é|e)|seacht|hocht|naoi|deich)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) countNumeralsMap >>= integer
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleAmhin
  , ruleCountNumerals
  , ruleDag
  , ruleDecimalNumeral
  , ruleDecimalWithThousandsSeparator
  , ruleIntegerWithThousandsSeparator
  , ruleNumerals
  , ruleNumerals2
  , ruleNumeralsPrefixWithNegativeOrMinus
  , ruleNumeralsSuffixesKMG
  , ruleOldVigesimalNumeralsS
  , ruleOldVigesimalFiche
  ]
