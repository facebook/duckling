-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Duckling.Ordinal.CA.Rules
  ( rules ) where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Ordinal.Helpers
import Duckling.Regex.Types
import Duckling.Types

ordinalsMap :: HashMap.HashMap Text.Text Int
ordinalsMap = HashMap.fromList
  [ ( "primer" , 1 )
  , ( "primers" , 1 )
  , ( "primera" , 1 )
  , ( "primeres" , 1 )
  , ( "segon" , 2 )
  , ( "segona" , 2 )
  , ( "segones" , 2 )
  , ( "segons" , 2 )
  , ( "tercer" , 3 )
  , ( "tercera" , 3 )
  , ( "tercers" , 3 )
  , ( "terceres" , 3 )
  , ( "quart" , 4 )
  , ( "quarta" , 4 )
  , ( "quarts" , 4 )
  , ( "quartes" , 4 )
  , ( "cinquè" , 5 )
  , ( "cinquena" , 5 )
  , ( "cinquens" , 5 )
  , ( "cinquenes" , 5 )
  , ( "sisè" , 6 )
  , ( "sisena" , 6 )
  , ( "sisens" , 6 )
  , ( "sisenes" , 6 )
  , ( "setè" , 7 )
  , ( "setena" , 7 )
  , ( "setens" , 7 )
  , ( "setenes" , 7 )
  , ( "vuitè" , 8 )
  , ( "vuitena" , 8 )
  , ( "vuitens" , 8 )
  , ( "vuitenes" , 8 )
  , ( "novè" , 9 )
  , ( "novena" , 9 )
  , ( "novens" , 9 )
  , ( "novenes" , 9 )
  , ( "desè" , 10 )
  , ( "desena" , 10 )
  , ( "desens" , 10 )
  , ( "desenes" , 10 )
  , ( "onzè" , 11 )
  , ( "onzena" , 11 )
  , ( "dotzè" , 12 )
  , ( "dotzena" , 12 )
  , ( "tretzè" , 13 )
  , ( "tretzena" , 13 )
  , ( "catorzè" , 14 )
  , ( "catorzena" , 14 )
  , ( "quinzè" , 15 )
  , ( "quinzena" , 15 )
  , ( "setzè" , 16 )
  , ( "setzena" , 16 )
  , ( "dissetè" , 17 )
  , ( "dissetena" , 17 )
  , ( "divuitè" , 18 )
  , ( "divuitena" , 18 )
  , ( "dinovè" , 19 )
  , ( "dinovena" , 19 )
  , ( "vintè" , 20 )
  , ( "vintena" , 20 )
  ]

ruleOrdinalsPrimero :: Rule
ruleOrdinalsPrimero = Rule
  { name = "ordinals (primero..10)"
  , pattern =
    [ regex "(primer(a|s|es)?|segon(a|s|es)?|tercer(a|s|es)?|quart(a|s|es)?|cinqu(è|(en(a|s|es)))|sis(è|en(a|s|es))|set(è|en(a|s|es))|vuit(è|en(a|s|es))|nov(è|en(a|s|es))|des(è|en(a|s|es))|onz(è|ena)|dotz(è|ena)|tretz(è|ena)|catorz(è|ena)|quinz(è|ena)|setz(è|ena)|disset(è|ena)|divuit(è|ena)|dinov(è|ena)|vint(è|ena))"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        ordinal <$> HashMap.lookup (Text.toLower match) ordinalsMap
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleOrdinalsPrimero
  ]
