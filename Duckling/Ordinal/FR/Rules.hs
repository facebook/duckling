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

import Data.HashMap.Strict ( HashMap)
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

ruleOrdinalsPremierseiziemeMap :: HashMap Text Int
ruleOrdinalsPremierseiziemeMap = HashMap.fromList
  [ ( "première"   , 1 )
  , ( "premiere"        , 1 )
  , ( "premier"         , 1 )
  , ( "deuxième"   , 2 )
  , ( "deuxieme"        , 2 )
  , ( "second"          , 2 )
  , ( "seconde"         , 2 )
  , ( "troisième"  , 3 )
  , ( "troisieme"       , 3 )
  , ( "quatrieme"       , 4 )
  , ( "quatrième"  , 4 )
  , ( "cinquieme"       , 5 )
  , ( "cinquième"  , 5 )
  , ( "sixième"    , 6 )
  , ( "sixieme"         , 6 )
  , ( "septieme"        , 7 )
  , ( "septième"   , 7 )
  , ( "huitième"   , 8 )
  , ( "huitieme"        , 8 )
  , ( "neuvieme"        , 9 )
  , ( "neuvième"   , 9 )
  , ( "dixième"    , 10 )
  , ( "dixieme"         , 10 )
  , ( "onzième"    , 11 )
  , ( "onzieme"         , 11 )
  , ( "douzieme"        , 12 )
  , ( "douzième"   , 12 )
  , ( "treizieme"       , 13 )
  , ( "treizième"  , 13 )
  , ( "quatorzième", 14 )
  , ( "quatorzieme"     , 14 )
  , ( "quinzième"  , 15 )
  , ( "quinzieme"       , 15 )
  , ( "seizieme"        , 16 )
  , ( "seizième"   , 16 )
  ]

ruleOrdinalsPremierseizieme :: Rule
ruleOrdinalsPremierseizieme = Rule
  { name = "ordinals (premier..seizieme)"
  , pattern =
    [ regex "(premi(ere?|ère)|(deux|trois|quatr|cinqu|six|sept|huit|neuv|dix|onz|douz|treiz|quatorz|quinz|seiz)i(e|è)me|seconde?)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        ordinal <$> HashMap.lookup (Text.toLower match) ruleOrdinalsPremierseiziemeMap
      _ -> Nothing
  }

ruleOrdinalDigits :: Rule
ruleOrdinalDigits = Rule
  { name = "ordinal (digits)"
  , pattern =
    [ regex "0*(\\d+) ?(ere?|ère|ème|eme|e)"
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
