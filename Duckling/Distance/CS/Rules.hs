-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Distance.CS.Rules
  ( rules ) where

import Data.String
import Prelude

import Duckling.Dimensions.Types
import Duckling.Distance.Helpers
import Duckling.Types
import qualified Duckling.Distance.Types as TDistance

ruleLatentDistKm :: Rule
ruleLatentDistKm = Rule
  { name = "<latent dist> km"
  , pattern =
    [ dimension Distance
    , regex "km|kilometr(y|\x016f)?"
    ]
  , prod = \tokens -> case tokens of
      (Token Distance dd:_) ->
        Just . Token Distance $ withUnit TDistance.Kilometre dd
      _ -> Nothing
  }

ruleDistMeters :: Rule
ruleDistMeters = Rule
  { name = "<dist> meters"
  , pattern =
    [ dimension Distance
    , regex "metr(y|\x016f)?|m"
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
    , regex "cm|centimetr(y|\x016f)?"
    ]
  , prod = \tokens -> case tokens of
      (Token Distance dd:_) ->
        Just . Token Distance $ withUnit TDistance.Centimetre dd
      _ -> Nothing
  }

ruleDistMiles :: Rule
ruleDistMiles = Rule
  { name = "<dist> miles"
  , pattern =
    [ dimension Distance
    , regex "mil"
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
