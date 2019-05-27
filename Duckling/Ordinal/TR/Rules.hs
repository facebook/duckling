-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


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
  , "üçüncü"
  , "dördüncü"
  , "beşinci"
  , "altıncı"
  , "yedinci"
  , "sekizinci"
  , "dokuzuncu"
  , "onuncu"
  , "on birinci"
  , "on ikinci"
  , "on üçüncü"
  , "on dördüncü"
  , "on beşinci"
  , "on altıncı"
  , "on yedinci"
  , "on sekizinci"
  , "on dokuzuncu"
  , "yirminci"
  , "yirmi birinci"
  , "yirmi ikinci"
  , "yirmi üçüncü"
  , "yirmi dördüncü"
  , "yirmi beşinci"
  , "yirmi altıncı"
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
    [ regex "0*(\\d+) ?('?)(inci|nci|ıncı|ncı|uncu|ncu|üncü|ncü|.)"
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
