-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.RO.Rules
  ( rules ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Ordinal.Helpers
import Duckling.Regex.Types
import Duckling.Types

rulePrimulDouaPatraNoua :: Rule
rulePrimulDouaPatraNoua = Rule
 { name = "special ordinals"
 , pattern =
   [ regex "(prim(a|ul)|a (patra|[dn]oua))"
   ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "prima"   -> Just $ ordinal 1
        "primul"  -> Just $ ordinal 1
        "a doua"  -> Just $ ordinal 2
        "a patra" -> Just $ ordinal 4
        "a noua"  -> Just $ ordinal 9
        _         -> Nothing
      _ -> Nothing
 }

-- We can't have a generic rule of the form "al?" + Numeral + "(le)?a"
-- It wouldn't match "al optlea"

ruleOrdinalDigits :: Rule
ruleOrdinalDigits = Rule
  { name = "ordinals (digits)"
  , pattern =
    [ regex "al?\\s0*(\\d+)[ -]?(le)?a"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        ordinal <$> parseInt match
      _ -> Nothing
  }

ordinalMap :: HashMap Text Int
ordinalMap = HashMap.fromList
  [ ("doi", 2)
  , ("trei", 3)
  , ("patru", 4)
  , ("cinci", 5)
  , ("sase", 6)
  , ("\537ase", 6)
  , ("sapte", 7)
  , ("\537apte", 7)
  , ("opt", 8)
  , ("noua", 9)
  , ("nouă", 9)
  ]

ruleSpelledOutOrdinals :: Rule
ruleSpelledOutOrdinals = Rule
  { name = "spelled out ordinals"
  , pattern =
    [ regex "al?\\s(doi|trei|patru|cinci|(s|ș)a(s|pt)e|opt|nou(a|ă))[ -]?(le)?a"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        ordinal <$> HashMap.lookup (Text.toLower match) ordinalMap
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleOrdinalDigits
  , rulePrimulDouaPatraNoua
  , ruleSpelledOutOrdinals
  ]
