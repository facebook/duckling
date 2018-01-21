-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.DA.Rules
  ( rules ) where

import qualified Data.Text as Text
import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Ordinal.Helpers
import Duckling.Regex.Types
import Duckling.Types

ruleOrdinalsFirstst :: Rule
ruleOrdinalsFirstst = Rule
  { name = "ordinals (first..31st)"
  , pattern =
    [ regex "(første|anden|tredje|fjerde|femte|sjette|syvende|ottende|niende|tiende|elfte|tolvte|trettende|fjortende|femtende|sekstende|syttende|attende|nittende|tyvende|tenogtyvende|toogtyvende|treogtyvende|fireogtyvende|femogtyvende|seksogtyvende|syvogtyvende|otteogtyvende|niogtyvende|tredivte|enogtredivte)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "første" -> Just $ ordinal 1
        "anden" -> Just $ ordinal 2
        "tredje" -> Just $ ordinal 3
        "fjerde" -> Just $ ordinal 4
        "femte" -> Just $ ordinal 5
        "sjette" -> Just $ ordinal 6
        "syvende" -> Just $ ordinal 7
        "ottende" -> Just $ ordinal 8
        "niende" -> Just $ ordinal 9
        "tiende" -> Just $ ordinal 10
        "elfte" -> Just $ ordinal 11
        "tolvte" -> Just $ ordinal 12
        "trettende" -> Just $ ordinal 13
        "fjortende" -> Just $ ordinal 14
        "femtende" -> Just $ ordinal 15
        "sekstende" -> Just $ ordinal 16
        "syttende" -> Just $ ordinal 17
        "attende" -> Just $ ordinal 18
        "nittende" -> Just $ ordinal 19
        "tyvende" -> Just $ ordinal 20
        "tenogtyvende" -> Just $ ordinal 21
        "toogtyvende" -> Just $ ordinal 22
        "treogtyvende" -> Just $ ordinal 23
        "fireogtyvende" -> Just $ ordinal 24
        "femogtyvende" -> Just $ ordinal 25
        "seksogtyvende" -> Just $ ordinal 26
        "syvogtyvende" -> Just $ ordinal 27
        "otteogtyvende" -> Just $ ordinal 28
        "niogtyvende" -> Just $ ordinal 29
        "tredivte" -> Just $ ordinal 30
        "enogtredivte" -> Just $ ordinal 31
        _ -> Nothing
      _ -> Nothing
  }

ruleOrdinalDigits :: Rule
ruleOrdinalDigits = Rule
  { name = "ordinal (digits)"
  , pattern =
    [ regex "0*(\\d+)(\\.|ste?)"
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
  , ruleOrdinalsFirstst
  ]
