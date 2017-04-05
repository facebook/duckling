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
  [ ( "premi\x00e8re"   , 1 )
  , ( "premiere"        , 1 )
  , ( "premier"         , 1 )
  , ( "deuxi\x00e8me"   , 2 )
  , ( "deuxieme"        , 2 )
  , ( "second"          , 2 )
  , ( "seconde"         , 2 )
  , ( "troisi\x00e8me"  , 3 )
  , ( "troisieme"       , 3 )
  , ( "quatrieme"       , 4 )
  , ( "quatri\x00e8me"  , 4 )
  , ( "cinquieme"       , 5 )
  , ( "cinqui\x00e8me"  , 5 )
  , ( "sixi\x00e8me"    , 6 )
  , ( "sixieme"         , 6 )
  , ( "septieme"        , 7 )
  , ( "septi\x00e8me"   , 7 )
  , ( "huiti\x00e8me"   , 8 )
  , ( "huitieme"        , 8 )
  , ( "neuvieme"        , 9 )
  , ( "neuvi\x00e8me"   , 9 )
  , ( "dixi\x00e8me"    , 10 )
  , ( "dixieme"         , 10 )
  , ( "onzi\x00e8me"    , 11 )
  , ( "onzieme"         , 11 )
  , ( "douzieme"        , 12 )
  , ( "douzi\x00e8me"   , 12 )
  , ( "treizieme"       , 13 )
  , ( "treizi\x00e8me"  , 13 )
  , ( "quatorzi\x00e8me", 14 )
  , ( "quatorzieme"     , 14 )
  , ( "quinzi\x00e8me"  , 15 )
  , ( "quinzieme"       , 15 )
  , ( "seizieme"        , 16 )
  , ( "seizi\x00e8me"   , 16 )
  ]

ruleOrdinalsPremierseizieme :: Rule
ruleOrdinalsPremierseizieme = Rule
  { name = "ordinals (premier..seizieme)"
  , pattern =
    [ regex "(premi(ere?|\x00e8re)|(deux|trois|quatr|cinqu|six|sept|huit|neuv|dix|onz|douz|treiz|quatorz|quinz|seiz)i(e|\x00e8)me|seconde?)"
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
