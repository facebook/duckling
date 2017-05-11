-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.DE.Rules
  ( rules ) where

import qualified Data.Text as Text
import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Ordinal.Helpers
import Duckling.Regex.Types
import Duckling.Types

ruleOrdinalsFirstth :: Rule
ruleOrdinalsFirstth = Rule
  { name = "ordinals (first..19th)"
  , pattern =
    [ regex "(erste(r|s)?|zweite(r|s)|dritte(r|s)|vierte(r|s)|fuenfte(r|s)|sechste(r|s)|siebte(r|s)|achte(r|s)|neunte(r|s)|zehnte(r|s)|elfter|zw\x00f6lfter|dreizenter|vierzehnter|f\x00fcnfzehnter|sechzenter|siebzehnter|achtzehnter|neunzehnter)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "erstes" -> Just $ ordinal 1
        "erster" -> Just $ ordinal 1
        "erste" -> Just $ ordinal 1
        "zweiter" -> Just $ ordinal 2
        "zweite" -> Just $ ordinal 2
        "zweites" -> Just $ ordinal 2
        "drittes" -> Just $ ordinal 3
        "dritte" -> Just $ ordinal 3
        "dritter" -> Just $ ordinal 3
        "viertes" -> Just $ ordinal 4
        "vierte" -> Just $ ordinal 4
        "vierter" -> Just $ ordinal 4
        "f\x00fcnftes" -> Just $ ordinal 5
        "f\x00fcnfter" -> Just $ ordinal 5
        "f\x00fcnfte" -> Just $ ordinal 5
        "sechste" -> Just $ ordinal 6
        "sechstes" -> Just $ ordinal 6
        "sechster" -> Just $ ordinal 6
        "siebtes" -> Just $ ordinal 7
        "siebte" -> Just $ ordinal 7
        "siebter" -> Just $ ordinal 7
        "achtes" -> Just $ ordinal 8
        "achte" -> Just $ ordinal 8
        "achter" -> Just $ ordinal 8
        "neuntes" -> Just $ ordinal 9
        "neunter" -> Just $ ordinal 9
        "neunte" -> Just $ ordinal 9
        "zehnte" -> Just $ ordinal 10
        "zehnter" -> Just $ ordinal 10
        "zehntes" -> Just $ ordinal 10
        "elfter" -> Just $ ordinal 11
        "zw\x00f6lfter" -> Just $ ordinal 12
        "dreizehnter" -> Just $ ordinal 13
        "vierzehnter" -> Just $ ordinal 14
        "f\x00fcnfzehnter" -> Just $ ordinal 15
        "sechzehnter" -> Just $ ordinal 16
        "siebzehnter" -> Just $ ordinal 17
        "achtzehnter" -> Just $ ordinal 18
        "neunzehnter" -> Just $ ordinal 19
        _ -> Nothing
      _ -> Nothing
  }

ruleOrdinalDigits :: Rule
ruleOrdinalDigits = Rule
  { name = "ordinal (digits)"
  , pattern =
    [ regex "(?<!\\d|\\.)0*(\\d+)(\\.(?!\\d)| ?(te(n|r|s)?)|(ste(n|r|s)?))"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        v <- parseInt match
        Just $ ordinal v
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleOrdinalDigits
  , ruleOrdinalsFirstth
  ]
