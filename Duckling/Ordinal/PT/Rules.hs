-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.PT.Rules
  ( rules ) where

import qualified Data.Text as Text
import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Ordinal.Helpers
import Duckling.Regex.Types
import Duckling.Types

ruleOrdinalsPrimeiro :: Rule
ruleOrdinalsPrimeiro = Rule
  { name = "ordinals (primeiro..10)"
  , pattern =
    [ regex "((primeir|segund|quart|quint|sext|s(e|\x00e9)tim|oitav|non|d(e|\x00e9)cim)(os?|as?))"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "primeira" -> Just $ ordinal 1
        "primeiros" -> Just $ ordinal 1
        "primeiras" -> Just $ ordinal 1
        "primeiro" -> Just $ ordinal 1
        "segundo" -> Just $ ordinal 2
        "segunda" -> Just $ ordinal 2
        "segundas" -> Just $ ordinal 2
        "segundos" -> Just $ ordinal 2
        "terceiros" -> Just $ ordinal 3
        "terceiro" -> Just $ ordinal 3
        "terceiras" -> Just $ ordinal 3
        "terceira" -> Just $ ordinal 3
        "quartos" -> Just $ ordinal 4
        "quarto" -> Just $ ordinal 4
        "quarta" -> Just $ ordinal 4
        "quartas" -> Just $ ordinal 4
        "quinto" -> Just $ ordinal 5
        "quinta" -> Just $ ordinal 5
        "quintas" -> Just $ ordinal 5
        "quintos" -> Just $ ordinal 5
        "sextos" -> Just $ ordinal 6
        "sexto" -> Just $ ordinal 6
        "sexta" -> Just $ ordinal 6
        "sextas" -> Just $ ordinal 6
        "setimas" -> Just $ ordinal 7
        "s\x00e9tima" -> Just $ ordinal 7
        "setimo" -> Just $ ordinal 7
        "setimos" -> Just $ ordinal 7
        "setima" -> Just $ ordinal 7
        "s\x00e9timos" -> Just $ ordinal 7
        "s\x00e9timo" -> Just $ ordinal 7
        "s\x00e9timas" -> Just $ ordinal 7
        "oitavas" -> Just $ ordinal 8
        "oitava" -> Just $ ordinal 8
        "oitavo" -> Just $ ordinal 8
        "oitavos" -> Just $ ordinal 8
        "nonos" -> Just $ ordinal 9
        "nona" -> Just $ ordinal 9
        "nono" -> Just $ ordinal 9
        "nonas" -> Just $ ordinal 9
        "d\x00e9cimos" -> Just $ ordinal 10
        "decimo" -> Just $ ordinal 10
        "decimos" -> Just $ ordinal 10
        "d\x00e9cimo" -> Just $ ordinal 10
        "decimas" -> Just $ ordinal 10
        "d\x00e9cima" -> Just $ ordinal 10
        "decima" -> Just $ ordinal 10
        "d\x00e9cimas" -> Just $ ordinal 10
        _ -> Nothing
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleOrdinalsPrimeiro
  ]
