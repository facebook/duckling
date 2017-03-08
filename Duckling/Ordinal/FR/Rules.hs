-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.FR.Rules
  ( rules ) where

import qualified Data.Text as Text
import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Number.Helpers (parseInt)
import Duckling.Ordinal.Helpers
import Duckling.Regex.Types
import Duckling.Types

ruleOrdinalsPremierseizieme :: Rule
ruleOrdinalsPremierseizieme = Rule
  { name = "ordinals (premier..seizieme)"
  , pattern =
    [ regex "(premi(ere?|\x00e8re)|(deux|trois|quatr|cinqu|six|sept|huit|neuv|dix|onz|douz|treiz|quatorz|quinz|seiz)i(e|\x00e8)me|seconde?)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "premi\x00e8re"    -> Just $ ordinal 1
        "premiere"         -> Just $ ordinal 1
        "premier"          -> Just $ ordinal 1
        "deuxi\x00e8me"    -> Just $ ordinal 2
        "deuxieme"         -> Just $ ordinal 2
        "second"           -> Just $ ordinal 2
        "seconde"          -> Just $ ordinal 2
        "troisi\x00e8me"   -> Just $ ordinal 3
        "troisieme"        -> Just $ ordinal 3
        "quatrieme"        -> Just $ ordinal 4
        "quatri\x00e8me"   -> Just $ ordinal 4
        "cinquieme"        -> Just $ ordinal 5
        "cinqui\x00e8me"   -> Just $ ordinal 5
        "sixi\x00e8me"     -> Just $ ordinal 6
        "sixieme"          -> Just $ ordinal 6
        "septieme"         -> Just $ ordinal 7
        "septi\x00e8me"    -> Just $ ordinal 7
        "huiti\x00e8me"    -> Just $ ordinal 8
        "huitieme"         -> Just $ ordinal 8
        "neuvieme"         -> Just $ ordinal 9
        "neuvi\x00e8me"    -> Just $ ordinal 9
        "dixi\x00e8me"     -> Just $ ordinal 10
        "dixieme"          -> Just $ ordinal 10
        "onzi\x00e8me"     -> Just $ ordinal 11
        "onzieme"          -> Just $ ordinal 11
        "douzieme"         -> Just $ ordinal 12
        "douzi\x00e8me"    -> Just $ ordinal 12
        "treizieme"        -> Just $ ordinal 13
        "treizi\x00e8me"   -> Just $ ordinal 13
        "quatorzi\x00e8me" -> Just $ ordinal 14
        "quatorzieme"      -> Just $ ordinal 14
        "quinzi\x00e8me"   -> Just $ ordinal 15
        "quinzieme"        -> Just $ ordinal 15
        "seizieme"         -> Just $ ordinal 16
        "seizi\x00e8me"    -> Just $ ordinal 16
        _                  -> Nothing
      _ -> Nothing
  }

ruleOrdinalDigits :: Rule
ruleOrdinalDigits = Rule
  { name = "ordinal (digits)"
  , pattern =
    [ regex "0*(\\d+) ?(ere?|\x00e8re|\x00e8me|eme|e)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        n <- parseInt match
        Just $ ordinal n
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleOrdinalDigits
  , ruleOrdinalsPremierseizieme
  ]
