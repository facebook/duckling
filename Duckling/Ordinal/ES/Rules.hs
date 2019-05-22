-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.ES.Rules
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
  , ( "primero" , 1 )
  , ( "primeros" , 1 )
  , ( "primera" , 1 )
  , ( "primeras" , 1 )
  , ( "segundo" , 2 )
  , ( "segunda" , 2 )
  , ( "segundas" , 2 )
  , ( "segundos" , 2 )
  , ( "terceros" , 3 )
  , ( "tercera" , 3 )
  , ( "terceras" , 3 )
  , ( "tercero" , 3 )
  , ( "tercer" , 3 )
  , ( "cuarta" , 4 )
  , ( "cuartas" , 4 )
  , ( "cuartos" , 4 )
  , ( "cuarto" , 4 )
  , ( "quinto" , 5 )
  , ( "quinta" , 5 )
  , ( "quintas" , 5 )
  , ( "quintos" , 5 )
  , ( "sextos" , 6 )
  , ( "sexto" , 6 )
  , ( "sexta" , 6 )
  , ( "sextas" , 6 )
  , ( "séptimas" , 7 )
  , ( "septimas" , 7 )
  , ( "séptima" , 7 )
  , ( "septimos" , 7 )
  , ( "septima" , 7 )
  , ( "séptimo" , 7 )
  , ( "séptimos" , 7 )
  , ( "septimo" , 7 )
  , ( "octavas" , 8 )
  , ( "octavo" , 8 )
  , ( "octavos" , 8 )
  , ( "octava" , 8 )
  , ( "novenos" , 9 )
  , ( "novena" , 9 )
  , ( "noveno" , 9 )
  , ( "novenas" , 9 )
  , ( "décimos" , 10 )
  , ( "decimo" , 10 )
  , ( "decimos" , 10 )
  , ( "décimo" , 10 )
  , ( "decimas" , 10 )
  , ( "décima" , 10 )
  , ( "decima" , 10 )
  , ( "décimas" , 10 )
  ]

ruleOrdinalsPrimero :: Rule
ruleOrdinalsPrimero = Rule
  { name = "ordinals (primero..10)"
  , pattern =
    [ regex "((primer|segund|cuart|quint|sext|s[eé]ptim|octav|noven|d[eé]cim)(os?|as?)|(prim|terc)er)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        ordinal <$> HashMap.lookup (Text.toLower match) ordinalsMap
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleOrdinalsPrimero
  ]
