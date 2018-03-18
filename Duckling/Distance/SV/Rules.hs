-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Distance.SV.Rules
  ( rules ) where

import Data.String
import Prelude

import Duckling.Dimensions.Types
import Duckling.Distance.Helpers
import qualified Duckling.Distance.Types as TDistance
import Duckling.Types

ruleLatentDistKm :: Rule
ruleLatentDistKm = Rule
  { name = "<latent dist> km"
  , pattern =
    [ dimension Distance
    , regex "k(ilo)?m?((e|é|è)ter)?s?"
    ]
  , prod = \tokens -> case tokens of
      (Token Distance dd:_) ->
        Just . Token Distance $ withUnit TDistance.Kilometer dd
      _ -> Nothing
  }

ruleDistMeters :: Rule
ruleDistMeters = Rule
  { name = "<dist> meter"
  , pattern =
    [ dimension Distance
    , regex "m((e|é|è)ters?)?"
    ]
  , prod = \tokens -> case tokens of
      (Token Distance dd:_) ->
        Just . Token Distance $ withUnit TDistance.Meter dd
      _ -> Nothing
  }

ruleDistCentimeters :: Rule
ruleDistCentimeters = Rule
  { name = "<dist> centimeters"
  , pattern =
    [ dimension Distance
    , regex "cm|centim(e|é|è)ters?"
    ]
  , prod = \tokens -> case tokens of
      (Token Distance dd:_) ->
        Just . Token Distance $ withUnit TDistance.Centimeter dd
      _ -> Nothing
  }

ruleDistMiles :: Rule
ruleDistMiles = Rule
  { name = "<dist> miles"
  , pattern =
    [ dimension Distance
    , regex "mile?s?"
    ]
  , prod = \tokens -> case tokens of
      (Token Distance dd:_) ->
        Just . Token Distance $ withUnit TDistance.Mile dd
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleDistCentimeters
  , ruleDistMeters
  , ruleDistMiles
  , ruleLatentDistKm
  ]
