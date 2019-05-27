-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Distance.SV.Rules
  ( rules
  ) where

import Data.String
import Prelude

import Duckling.Dimensions.Types
import Duckling.Distance.Helpers
import Duckling.Distance.Types (DistanceData(..))
import Duckling.Types
import qualified Duckling.Distance.Types as TDistance

ruleLatentDistKm :: Rule
ruleLatentDistKm = Rule
  { name = "<latent dist> km"
  , pattern =
    [ dimension Distance
    , regex "k(ilo)?m?(eter)?"
    ]
  , prod = \tokens -> case tokens of
      (Token Distance dd:_) ->
        Just . Token Distance $ withUnit TDistance.Kilometre dd
      _ -> Nothing
  }

ruleDistMeters :: Rule
ruleDistMeters = Rule
  { name = "<dist> meter"
  , pattern =
    [ dimension Distance
    , regex "m(eter)?"
    ]
  , prod = \tokens -> case tokens of
      (Token Distance dd:_) ->
        Just . Token Distance $ withUnit TDistance.Metre dd
      _ -> Nothing
  }

ruleDistCentimeters :: Rule
ruleDistCentimeters = Rule
  { name = "<dist> centimeters"
  , pattern =
    [ dimension Distance
    , regex "cm|centimeter"
    ]
  , prod = \tokens -> case tokens of
      (Token Distance dd:_) ->
        Just . Token Distance $ withUnit TDistance.Centimetre dd
      _ -> Nothing
  }

ruleDistMils :: Rule
ruleDistMils = Rule
  { name = "<dist> mils"
  , pattern =
    [ dimension Distance
    , regex "mils?"
    ]
  , prod = \tokens -> case tokens of
      (Token Distance dd@DistanceData {TDistance.value = Just x}:_) ->
        Just . Token Distance . withValue (10 * x)
          $ withUnit TDistance.Kilometre dd
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleDistCentimeters
  , ruleDistMeters
  , ruleDistMils
  , ruleLatentDistKm
  ]
