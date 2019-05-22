-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.NB.Rules
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
    [ regex "(første|andre|tredje|fjerde|femtende|femte|sjette|syvende|åttende|niende|tiende|ellevte|tolvte|trettende|fjortende|sekstende|syttende|attende|nittende|tyvende|tjuende|enogtyvende|toogtyvende|treogtyvende|fireogtyvende|femogtyvende|seksogtyvende|syvogtyvende|åtteogtyvende|niogtyvende|enogtjuende|toogtjuende|treogtjuende|fireogtjuende|femogtjuende|seksogtjuende|syvogtjuende|åtteogtyvend|niogtjuende|tredefte|enogtredefte)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "første" -> Just $ ordinal 1
        "andre" -> Just $ ordinal 2
        "tredje" -> Just $ ordinal 3
        "fjerde" -> Just $ ordinal 4
        "femte" -> Just $ ordinal 5
        "sjette" -> Just $ ordinal 6
        "syvende" -> Just $ ordinal 7
        "åttende" -> Just $ ordinal 8
        "niende" -> Just $ ordinal 9
        "tiende" -> Just $ ordinal 10
        "ellevte" -> Just $ ordinal 11
        "tolvte" -> Just $ ordinal 12
        "trettende" -> Just $ ordinal 13
        "fjortende" -> Just $ ordinal 14
        "femtende" -> Just $ ordinal 15
        "sekstende" -> Just $ ordinal 16
        "syttende" -> Just $ ordinal 17
        "attende" -> Just $ ordinal 18
        "nittende" -> Just $ ordinal 19
        "tyvende" -> Just $ ordinal 20
        "tjuende" -> Just $ ordinal 20
        "enogtjuende" -> Just $ ordinal 21
        "enogtyvende" -> Just $ ordinal 21
        "toogtyvende" -> Just $ ordinal 22
        "toogtjuende" -> Just $ ordinal 22
        "treogtyvende" -> Just $ ordinal 23
        "treogtjuende" -> Just $ ordinal 23
        "fireogtjuende" -> Just $ ordinal 24
        "fireogtyvende" -> Just $ ordinal 24
        "femogtyvende" -> Just $ ordinal 25
        "femogtjuende" -> Just $ ordinal 25
        "seksogtjuende" -> Just $ ordinal 26
        "seksogtyvende" -> Just $ ordinal 26
        "syvogtyvende" -> Just $ ordinal 27
        "syvogtjuende" -> Just $ ordinal 27
        "åtteogtyvende" -> Just $ ordinal 28
        "åtteogtjuende" -> Just $ ordinal 28
        "niogtyvende" -> Just $ ordinal 29
        "niogtjuende" -> Just $ ordinal 29
        "tredefte" -> Just $ ordinal 30
        "enogtredefte" -> Just $ ordinal 31
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
      (Token RegexMatch (GroupMatch (match:_)):_) -> ordinal <$> parseInt match
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleOrdinalDigits
  , ruleOrdinalsFirstst
  ]
