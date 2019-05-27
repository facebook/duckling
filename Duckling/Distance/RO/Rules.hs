-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Distance.RO.Rules
  ( rules
  ) where

import Data.HashMap.Strict (HashMap)
import Data.String
import Data.Text (Text)
import Prelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Distance.Helpers
import Duckling.Regex.Types
import Duckling.Types
import qualified Duckling.Distance.Types as TDistance

unitMap :: HashMap Text TDistance.Unit
unitMap = HashMap.fromList
  [ ( "cm", TDistance.Centimetre )
  , ( "centimetri", TDistance.Centimetre )
  , ( "centimetru", TDistance.Centimetre )
  , ( "picior", TDistance.Foot )
  , ( "picioare", TDistance.Foot )
  , ( "inch", TDistance.Inch )
  , ( "inchi", TDistance.Inch )
  , ( "inci", TDistance.Inch )
  , ( "km", TDistance.Kilometre )
  , ( "kilometri", TDistance.Kilometre )
  , ( "kilometru", TDistance.Kilometre )
  , ( "m", TDistance.Metre )
  , ( "metri", TDistance.Metre )
  , ( "metru", TDistance.Metre )
  , ( "mila", TDistance.Mile )
  , ( "milă", TDistance.Mile )
  , ( "mile", TDistance.Mile )
  , ( "y", TDistance.Yard )
  , ( "yar", TDistance.Yard )
  , ( "yard", TDistance.Yard )
  , ( "yarzi", TDistance.Yard )
  , ( "yd", TDistance.Yard )
  , ( "yzi", TDistance.Yard )
  ]

ruleLatentDistUnit :: Rule
ruleLatentDistUnit = Rule
  { name = "<latent dist> foot/inch/yard/meter/kilometer/centimeter"
  , pattern =
    [ dimension Distance
    , regex "(inc(hi?|i)|(centi|kilo)?metr[iu]|mil[eaă]|[ck]?m|picio(are|r)|y(ar)?(zi|d)?)"
    ]
  , prod = \case
      (Token Distance dd:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> do
         x <- HashMap.lookup (Text.toLower match) unitMap
         Just . Token Distance $ withUnit x dd
      _ -> Nothing
  }

ruleLatentDistDeUnit :: Rule
ruleLatentDistDeUnit = Rule
  { name = "<latent dist> foot/inch/yard/meter/kilometer/centimeter"
  , pattern =
    [ dimension Distance
    , regex "de (inc(hi?|i)|(centi|kilo)?metr[iu]|mil[eaă]|[ck]?m|picio(are|r)|y(ar)?(zi|d)?)"
    ]
  , prod = \case
      (Token Distance dd:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> do
         x <- HashMap.lookup (Text.toLower match) unitMap
         Just . Token Distance $ withUnit x dd
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleLatentDistUnit
  , ruleLatentDistDeUnit
  ]
