-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Distance.GA.Rules
  ( rules ) where

import Data.String
import Prelude

import Duckling.Dimensions.Types
import Duckling.Distance.Helpers
import qualified Duckling.Distance.Types as TDistance
import Duckling.Types

ruleDistMeters :: Rule
ruleDistMeters = Rule
  { name = "<dist> meters"
  , pattern =
    [ dimension Distance
    , regex "mh?(e|é)adai?r"
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
    , regex "(c\\.?m\\.?|g?ch?eintimh?(e|é)adai?r)"
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
    , regex "mh?(í|i)lt?e"
    ]
  , prod = \tokens -> case tokens of
      (Token Distance dd:_) ->
        Just . Token Distance $ withUnit TDistance.Mile dd
      _ -> Nothing
  }

ruleLatentDistKm :: Rule
ruleLatentDistKm = Rule
  { name = "<latent dist> km"
  , pattern =
    [ dimension Distance
    , regex "(k\\.?(m\\.?)?|g?ch?ilim(e|é)adai?r)"
    ]
  , prod = \tokens -> case tokens of
      (Token Distance dd:_) ->
        Just . Token Distance $ withUnit TDistance.Kilometre dd
      _ -> Nothing
  }

ruleLatentDistTroigh :: Rule
ruleLatentDistTroigh = Rule
  { name = "<latent dist> troigh"
  , pattern =
    [ dimension Distance
    , regex "('|d?th?roi[tg]he?|tr\\.?)"
    ]
  , prod = \tokens -> case tokens of
      (Token Distance dd:_) ->
        Just . Token Distance $ withUnit TDistance.Foot dd
      _ -> Nothing
  }

ruleLatentDistOrlach :: Rule
ruleLatentDistOrlach = Rule
  { name = "<latent dist> orlach"
  , pattern =
    [ dimension Distance
    , regex "(''|([nth]-?)?orl(ach|aigh|a(í|i)|\\.))"
    ]
  , prod = \tokens -> case tokens of
      (Token Distance dd:_) ->
        Just . Token Distance $ withUnit TDistance.Inch dd
      _ -> Nothing
  }

ruleDistMAmbiguousMilesOrMeters :: Rule
ruleDistMAmbiguousMilesOrMeters = Rule
  { name = "<dist> m (ambiguous miles or meters)"
  , pattern =
    [ dimension Distance
    , regex "m"
    ]
  , prod = \tokens -> case tokens of
      (Token Distance dd:_) ->
        Just . Token Distance $ withUnit TDistance.M dd
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleDistCentimeters
  , ruleDistMAmbiguousMilesOrMeters
  , ruleDistMeters
  , ruleDistMiles
  , ruleLatentDistKm
  , ruleLatentDistOrlach
  , ruleLatentDistTroigh
  ]
