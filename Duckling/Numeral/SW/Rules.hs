-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Numeral.SW.Rules
  ( rules
  ) where

import Data.HashMap.Strict (HashMap)
import Data.Maybe
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

ruleNumeralMap :: HashMap Text Integer
ruleNumeralMap = HashMap.fromList
  [ ( "sufuri", 0 )
  , ( "zero", 0 )
  , ( "moja", 1 )
  , ( "mbili", 2 )
  , ( "tatu", 3 )
  , ( "nne", 4 )
  , ( "tano", 5)
  , ( "sita", 6)
  , ( "saba", 7)
  , ( "nane", 8)
  , ( "tisa", 9)
  , ( "kumi", 10)
  ]

ruleNumeral :: Rule
ruleNumeral = Rule
  { name = "number (0..10)"
  , pattern =
    [ regex "(sufuri|zero|moja|mbili|tatu|nne|tano|sita|saba|nane|tisa|kumi)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) ruleNumeralMap >>= integer
      _ -> Nothing
  }

tensMap :: HashMap Text Integer
tensMap = HashMap.fromList
  [ ( "ishirini", 20 )
  , ( "thelathini", 30 )
  , ( "arubaini", 40 )
  , ( "arobaini", 40 )
  , ( "hamsini", 50 )
  , ( "sitini", 60 )
  , ( "sabini", 70 )
  , ( "themanini", 80 )
  , ( "tisini", 90 )
  ]

ruleTens :: Rule
ruleTens = Rule
  { name = "integer (20,30..90)"
  , pattern =
    [ regex "(ishirini|thelathini|arubaini|arobaini|hamsini|sitini|sabini|themanini|tisini)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) tensMap >>= integer
      _ -> Nothing
  }


ruleCompositeTens :: Rule
ruleCompositeTens = Rule
  { name = "integer 11..19 21..29 .. 91..99"
  , pattern = [oneOf [20,30..90]
  , regex "-?na-?"
  , numberBetween 1 10]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v1}:
       Token Numeral NumeralData{TNumeral.value = v2}:
       _) -> double $ v1 + v2
      _ -> Nothing
  }


rules :: [Rule]
rules =
  [ ruleNumeral
  , ruleTens
  , ruleCompositeTens
  ]
