-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.HU.Rules
  ( rules ) where

import Control.Monad (join)
import Data.HashMap.Strict ( HashMap)
import Data.String
import Data.Text (Text)
import Prelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Ordinal.Helpers
import Duckling.Regex.Types
import Duckling.Types

ordinalsMap :: HashMap Text Int
ordinalsMap = HashMap.fromList
  [ ( "első", 1 )
  , ( "második", 2 )
  , ( "harmadik", 3 )
  , ( "negyedik", 4 )
  , ( "ötödik", 5 )
  , ( "hatodik", 6 )
  , ( "hetedik", 7 )
  , ( "nyolcadik", 8 )
  , ( "kilencedik", 9 )
  , ( "tizedik", 10 )
  , ( "huszadik", 20 )
  , ( "harmincadik", 30 )
  , ( "negyvenedik", 40 )
  , ( "ötvenedik", 50 )
  , ( "hatvanadik", 60 )
  , ( "hetvenedik", 70 )
  , ( "nyolcvanadik", 80 )
  , ( "kilencvenedik", 90 )
  ]

ordinalsMap2 :: HashMap Text Int
ordinalsMap2 = HashMap.fromList
    [ ( "egyedik", 1 )
    , ( "kettedik", 2 )
    , ( "harmadik", 3 )
    , ( "negyedik", 4 )
    , ( "ötödik", 5 )
    , ( "hatodik", 6 )
    , ( "hetedik", 7 )
    , ( "nyolcadik", 8 )
    , ( "kilencedik", 9 )
    ]

cardinalsMap :: HashMap Text Int
cardinalsMap = HashMap.fromList
  [ ( "tizen", 10 )
  , ( "huszon", 20 )
  , ( "harminc", 30 )
  , ( "negyven", 40 )
  , ( "ötven", 50 )
  , ( "hatvan", 60 )
  , ( "hetven", 70 )
  , ( "nyolcvan", 80 )
  , ( "kilencven", 90 )
  ]

ruleOrdinals :: Rule
ruleOrdinals = Rule
  { name = "ordinals (first..twentieth,thirtieth,...)"
  , pattern =
    [ regex "(első|második|harmadik|negyedik|ötödik|hatodik|hetedik|nyolcadik|kilencedik|tizedik|huszadik|harmincadik|negyvenedik|ötvenedik|hatvanadik|hetvenedik|nyolcvanadik|kilencvenedik)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        ordinal <$> HashMap.lookup (Text.toLower match) ordinalsMap
      _ -> Nothing
    }

ruleCompositeOrdinals :: Rule
ruleCompositeOrdinals = Rule
  { name = "ordinals (composite, e.g., eighty-seven)"
  , pattern =
    [ regex "(tizen|huszon|harminc|negyven|ötven|hatvan|hetven|nyolcvan|kilencven)\\-?(egyedik|kettedik|harmadik|negyedik|ötödik|hatodik|hetedik|nyolcadik|kilencedik)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (tens:units:_)):_) -> do
        tt <- HashMap.lookup (Text.toLower tens) cardinalsMap
        uu <- HashMap.lookup (Text.toLower units) ordinalsMap2
        Just . ordinal $ tt + uu
      _ -> Nothing
  }

ruleOrdinalDigits :: Rule
ruleOrdinalDigits = Rule
  { name = "ordinal (digits)"
  , pattern =
    [ regex "0*(\\d+)\\."
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> ordinal <$> parseInt match
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleOrdinals
  , ruleCompositeOrdinals
  , ruleOrdinalDigits
  ]
