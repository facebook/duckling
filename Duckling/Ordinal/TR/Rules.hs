-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.TR.Rules
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

ordinals :: [Text]
ordinals =
  [ "birinci"
  , "ikinci"
  , "\x00fc\x00e7\x00fcnc\x00fc"
  , "d\x00f6rd\x00fcnc\x00fc"
  , "be\x015finci"
  , "alt\x0131nc\x0131"
  , "yedinci"
  , "sekizinci"
  , "dokuzuncu"
  , "onuncu"
  , "on birinci"
  , "on ikinci"
  , "on \x00fc\x00e7\x00fcnc\x00fc"
  , "on d\x00f6rd\x00fcnc\x00fc"
  , "on be\x015finci"
  , "on alt\x0131nc\x0131"
  , "on yedinci"
  , "on sekizinci"
  , "on dokuzuncu"
  , "yirminci"
  , "yirmi birinci"
  , "yirmi ikinci"
  , "yirmi \x00fc\x00e7\x00fcnc\x00fc"
  , "yirmi d\x00f6rd\x00fcnc\x00fc"
  , "yirmi be\x015finci"
  , "yirmi alt\x0131nc\x0131"
  , "yirmi yedinci"
  , "yirmi sekizinci"
  , "yirmi dokuzuncu"
  , "otuzuncu"
  , "otuz birinci"
  ]

ordinalsHashMap :: HashMap Text Int
ordinalsHashMap = HashMap.fromList $ zip ordinals [1..]

ruleOrdinalsFirstst :: Rule
ruleOrdinalsFirstst = Rule
  { name = "ordinals (first..31st)"
  , pattern =
    [ regex . Text.unpack $
        Text.concat [ "(", Text.intercalate "|" ordinals, ")" ]
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        ordinal <$> HashMap.lookup (Text.toLower match) ordinalsHashMap
      _ -> Nothing
  }

ruleOrdinalDigits :: Rule
ruleOrdinalDigits = Rule
  { name = "ordinal (digits)"
  , pattern =
    [ regex "0*(\\d+) ?('?)(inci|nci|\x0131nc\x0131|nc\x0131|uncu|ncu|\x00fcnc\x00fc|nc\x00fc|.)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        ordinal <$> parseInt match
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleOrdinalDigits
  , ruleOrdinalsFirstst
  ]
