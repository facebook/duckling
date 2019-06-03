-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.ID.Rules
  ( rules ) where

import qualified Data.Text as Text
import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Ordinal.Helpers
import Duckling.Regex.Types
import Duckling.Types

ruleOrdinals :: Rule
ruleOrdinals = Rule
  { name = "ordinals"
  , pattern =
    [ regex "(pertama|kedua|ketiga|keempat|kelima|keenam|ketujuh|kedelapan|kesembilan|kesepuluh)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "pertama" -> Just $ ordinal 1
        "kedua" -> Just $ ordinal 2
        "ketiga" -> Just $ ordinal 3
        "keempat" -> Just $ ordinal 4
        "kelima" -> Just $ ordinal 5
        "keenam" -> Just $ ordinal 6
        "ketujuh" -> Just $ ordinal 7
        "kedelapan" -> Just $ ordinal 8
        "kesembilan" -> Just $ ordinal 9
        "kesepuluh" -> Just $ ordinal 10
        _ -> Nothing
      _ -> Nothing
  }

ruleOrdinalsDigits :: Rule
ruleOrdinalsDigits = Rule
  { name = "ordinals (digits)"
  , pattern =
    [ regex "ke-0*(\\d+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> ordinal <$> parseInt match
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleOrdinals
  , ruleOrdinalsDigits
  ]
