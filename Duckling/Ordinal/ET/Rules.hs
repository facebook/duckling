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
    [ regex "(esimene|teine|kolmas|neljas|viies|kuues|seitsmes|kaheksas|\x00fcheksas|k\x00fcmnes|\x00fcheteistk\x00fcmnes|kaheteistk\x00fcmnes|kolmeteistk\x00fcmnes|neljateistk\x00fcmnes|viieteistk\x00fcmnes|kuueteistk\x00fcmnes|seitsmeteistk\x00fcmnes|kaheksateistk\x00fcmnes|\x00fcheksateistk\x00fcmnes)"
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
        "\x00fcheksas" -> Just $ ordinal 9
        "k\x00fcmnes" -> Just $ ordinal 10
        "\x00fcheteistk\x00fcmnes" -> Just $ ordinal 11
        "kaheteistk\x00fcmnes" -> Just $ ordinal 12
        "kolmeteistk\x00fcmnes" -> Just $ ordinal 13
        "neljateistk\x00fcmnes" -> Just $ ordinal 14
        "viieteistk\x00fcmnes" -> Just $ ordinal 15
        "kuueteistk\x00fcmnes" -> Just $ ordinal 16
        "seitsmeteistk\x00fcmnes" -> Just $ ordinal 17
        "kaheksateistk\x00fcmnes" -> Just $ ordinal 18
        "\x00fcheksateistk\x00fcmnes" -> Just $ ordinal 19
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
