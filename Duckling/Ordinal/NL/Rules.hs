-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.NL.Rules
  ( rules ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text as Text
import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Ordinal.Helpers
import Duckling.Regex.Types
import Duckling.Types

zeroNineteenMap :: HashMap Text Int
zeroNineteenMap = HashMap.fromList
  [ ("eerste", 1)
  , ("tweede", 2)
  , ("derde", 3)
  , ("vierde", 4)
  , ("vijfde", 5)
  , ("zesde", 6)
  , ("zevende", 7)
  , ("achste", 8)
  , ("negende", 9)
  , ("tiende", 10)
  , ("elfde", 11)
  , ("twaalfde", 12)
  , ("dertiende", 13)
  , ("veertiende", 14)
  , ("vijftiende", 15)
  , ("zestiende", 16)
  , ("zeventiende", 17)
  , ("achtiende", 18)
  , ("negentiende", 19)
  ]

ruleOrdinalsFirstth :: Rule
ruleOrdinalsFirstth = Rule
  { name = "ordinals (first..19th)"
  , pattern =
    [ regex "(eerste|tweede|derde|vierde|vijfde|zeste|zevende|achtste|negende|tiende|elfde|twaalfde|veertiende|vijftiende|zestiende|zeventiende|achtiende|negentiende)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        ordinal <$> HashMap.lookup (Text.toLower match) zeroNineteenMap
      _ -> Nothing
  }

ruleOrdinalDigits :: Rule
ruleOrdinalDigits = Rule
  { name = "ordinal (digits)"
  , pattern =
    [ regex "0*(\\d+)(\\.| ?(ste|de))"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        ordinal <$> parseInt match
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleOrdinalDigits
  , ruleOrdinalsFirstth
  ]
