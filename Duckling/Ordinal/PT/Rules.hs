-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.PT.Rules
  ( rules
  ) where

import Data.HashMap.Strict (HashMap)
import Data.String
import Data.Text (Text)
import Prelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Ordinal.Helpers
import Duckling.Ordinal.Types (OrdinalData (..))
import Duckling.Regex.Types (GroupMatch (..))
import Duckling.Types
import qualified Duckling.Ordinal.Types as TOrdinal

ordinalsMap :: HashMap Text Int
ordinalsMap = HashMap.fromList
  [ ( "primeir", 1 )
  , ( "segund", 2 )
  , ( "terceir", 3 )
  , ( "quart", 4 )
  , ( "quint", 5 )
  , ( "sext", 6 )
  , ( "setim", 7 )
  , ( "sétim", 7 )
  , ( "oitav", 8 )
  , ( "non", 9 )
  , ( "decim", 10 )
  , ( "décim", 10 )
  ]

ruleOrdinals :: Rule
ruleOrdinals = Rule
  { name = "ordinals (1..10)"
  , pattern =
    [ regex "(primeir|segund|terceir|quart|quint|sext|s[ée]tim|oitav|non|d[ée]cim)[ao]s?"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        ordinal <$> HashMap.lookup (Text.toLower match) ordinalsMap
      _ -> Nothing
  }

cardinalsMap :: HashMap Text Int
cardinalsMap = HashMap.fromList
  [ ( "vi", 20 )
  , ( "tri", 30 )
  , ( "quadra", 40 )
  , ( "qüinqua", 50 )
  , ( "quinqua", 50 )
  , ( "sexa", 60 )
  , ( "septua", 70 )
  , ( "octo", 80 )
  , ( "nona", 90 )
  ]

ruleCardinals :: Rule
ruleCardinals = Rule
  { name = "cardinals (20 .. 90)"
  , pattern =
    [ regex "(vi|tri|quadra|q[üu]inqua|sexa|septua|octo|nona)g[ée]sim[ao]s?"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        ordinal <$> HashMap.lookup (Text.toLower match) cardinalsMap
      _ -> Nothing
  }

ruleCompositeOrdinals :: Rule
ruleCompositeOrdinals = Rule
  { name = "ordinals (11..19)"
  , pattern =
    [ oneOf [10, 20 .. 90]
    , oneOf [1..9]
    ]
  , prod = \case
      (Token Ordinal OrdinalData{TOrdinal.value = t}:
       Token Ordinal OrdinalData{TOrdinal.value = u}:
       _) -> Just $ ordinal $ t + u
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleOrdinals
  , ruleCardinals
  , ruleCompositeOrdinals
  ]
