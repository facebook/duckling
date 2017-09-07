-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.ET.Rules
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
    [ regex "(esimene|teine|kolmas|neljas|viies|kuues|seitsmes|kaheksas|üheksas|kümnes|üheteistkümnes|kaheteistkümnes|kolmeteistkümnes|neljateistkümnes|viieteistkümnes|kuueteistkümnes|seitsmeteistkümnes|kaheksateistkümnes|üheksateistkümnes)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "esimene" -> Just $ ordinal 1
        "teine" -> Just $ ordinal 2
        "kolmas" -> Just $ ordinal 3
        "neljas" -> Just $ ordinal 4
        "viies" -> Just $ ordinal 5
        "kuues" -> Just $ ordinal 6
        "seitsmes" -> Just $ ordinal 7
        "kaheksas" -> Just $ ordinal 8
        "üheksas" -> Just $ ordinal 9
        "kümnes" -> Just $ ordinal 10
        "üheteistkümnes" -> Just $ ordinal 11
        "kaheteistkümnes" -> Just $ ordinal 12
        "kolmeteistkümnes" -> Just $ ordinal 13
        "neljateistkümnes" -> Just $ ordinal 14
        "viieteistkümnes" -> Just $ ordinal 15
        "kuueteistkümnes" -> Just $ ordinal 16
        "seitsmeteistkümnes" -> Just $ ordinal 17
        "kaheksateistkümnes" -> Just $ ordinal 18
        "üheksateistkümnes" -> Just $ ordinal 19
        _ -> Nothing
      _ -> Nothing
  }

ruleOrdinalDigits :: Rule
ruleOrdinalDigits = Rule
  { name = "ordinal (digits)"
  , pattern =
    [ regex "0*(\\d+)\\."
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> ordinal <$> parseInt match
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleOrdinalDigits
  , ruleOrdinalsFirstth
  ]
