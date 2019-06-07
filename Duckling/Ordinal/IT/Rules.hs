-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.IT.Rules
  ( rules ) where

import qualified Data.Text as Text
import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Ordinal.Helpers
import Duckling.Regex.Types
import Duckling.Types

ruleOrdinalsPrimo :: Rule
ruleOrdinalsPrimo = Rule
  { name = "ordinals (primo..10)"
  , pattern =
    [ regex "((prim|second|terz|quart|quint|sest|settim|ottav|non|decim)(o|a|i|e))"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "primi" -> Just $ ordinal 1
        "prima" -> Just $ ordinal 1
        "primo" -> Just $ ordinal 1
        "prime" -> Just $ ordinal 1
        "seconda" -> Just $ ordinal 2
        "secondi" -> Just $ ordinal 2
        "seconde" -> Just $ ordinal 2
        "secondo" -> Just $ ordinal 2
        "terze" -> Just $ ordinal 3
        "terzi" -> Just $ ordinal 3
        "terzo" -> Just $ ordinal 3
        "terza" -> Just $ ordinal 3
        "quarte" -> Just $ ordinal 4
        "quarto" -> Just $ ordinal 4
        "quarta" -> Just $ ordinal 4
        "quarti" -> Just $ ordinal 4
        "quinto" -> Just $ ordinal 5
        "quinta" -> Just $ ordinal 5
        "quinti" -> Just $ ordinal 5
        "quinte" -> Just $ ordinal 5
        "sesti" -> Just $ ordinal 6
        "seste" -> Just $ ordinal 6
        "sesta" -> Just $ ordinal 6
        "sesto" -> Just $ ordinal 6
        "settimi" -> Just $ ordinal 7
        "settima" -> Just $ ordinal 7
        "settimo" -> Just $ ordinal 7
        "settime" -> Just $ ordinal 7
        "ottavo" -> Just $ ordinal 8
        "ottava" -> Just $ ordinal 8
        "ottavi" -> Just $ ordinal 8
        "ottave" -> Just $ ordinal 8
        "none" -> Just $ ordinal 9
        "noni" -> Just $ ordinal 9
        "nona" -> Just $ ordinal 9
        "nono" -> Just $ ordinal 9
        "decimo" -> Just $ ordinal 10
        "decima" -> Just $ ordinal 10
        "decime" -> Just $ ordinal 10
        "decimi" -> Just $ ordinal 10
        _ -> Nothing
      _ -> Nothing
  }

ruleOrdinalDigits :: Rule
ruleOrdinalDigits = Rule
  { name = "ordinal (digits)"
  , pattern =
    [ regex "0*(\\d+) ?(ª|°|°)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> ordinal <$> parseInt match
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleOrdinalDigits
  , ruleOrdinalsPrimo
  ]
