-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Distance.HR.Rules
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
    , regex "k(ilo)?m?(eta?r)?a?"
    ]
  , prod = \tokens -> case tokens of
      (Token Distance dd:_) ->
        Just . Token Distance $ withUnit TDistance.Kilometre dd
      _ -> Nothing
  }

ruleDistMetar :: Rule
ruleDistMetar = Rule
  { name = "<dist> metar"
  , pattern =
    [ dimension Distance
    , regex "metara?"
    ]
  , prod = \tokens -> case tokens of
      (Token Distance dd:_) ->
        Just . Token Distance $ withUnit TDistance.Metre dd
      _ -> Nothing
  }

ruleDistCentimetar :: Rule
ruleDistCentimetar = Rule
  { name = "<dist> centimetar"
  , pattern =
    [ dimension Distance
    , regex "cm|centimeta?ra?"
    ]
  , prod = \tokens -> case tokens of
      (Token Distance dd:_) ->
        Just . Token Distance $ withUnit TDistance.Centimetre dd
      _ -> Nothing
  }

ruleDistMilja :: Rule
ruleDistMilja = Rule
  { name = "<dist> milja"
  , pattern =
    [ dimension Distance
    , regex "milja"
    ]
  , prod = \tokens -> case tokens of
      (Token Distance dd:_) -> Just . Token Distance $ withUnit TDistance.Mile dd
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
      (Token Distance dd:_) -> Just . Token Distance $ withUnit TDistance.M dd
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleDistCentimetar
  , ruleDistMAmbiguousMilesOrMeters
  , ruleDistMetar
  , ruleDistMilja
  , ruleLatentDistKm
  ]
