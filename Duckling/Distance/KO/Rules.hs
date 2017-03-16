-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Distance.KO.Rules
  ( rules ) where

import Data.String
import Prelude

import Duckling.Dimensions.Types
import Duckling.Distance.Helpers
import qualified Duckling.Distance.Types as TDistance
import Duckling.Numeral.Types (NumeralData (..))
import qualified Duckling.Numeral.Types as TNumeral
import Duckling.Types

ruleLatentDistYard :: Rule
ruleLatentDistYard = Rule
  { name = "<latent dist> yard"
  , pattern =
    [ dimension Distance
    , regex "y(ar)?ds?|\xc57c\xb4dc"
    ]
  , prod = \tokens -> case tokens of
      (Token Distance dd:_) ->
        Just . Token Distance $ withUnit TDistance.Yard dd
      _ -> Nothing
  }

ruleDistCentimeters :: Rule
ruleDistCentimeters = Rule
  { name = "<dist> centimeters"
  , pattern =
    [ dimension Distance
    , regex "cm|\xc13c(\xd2f0|\xce58)((\xbbf8|\xba54)\xd130)?"
    ]
  , prod = \tokens -> case tokens of
      (Token Distance dd:_) ->
        Just . Token Distance $ withUnit TDistance.Centimetre dd
      _ -> Nothing
  }

ruleLatentDistFeetAndLatentDistInch :: Rule
ruleLatentDistFeetAndLatentDistInch = Rule
  { name = "<latent dist> feet and <latent dist> inch "
  , pattern =
    [ dimension Distance
    , regex "('|f(oo|ee)?ts?)|\xd53c\xd2b8"
    , dimension Distance
    , regex "(''|inch(es)?)|\xc778\xce58"
    ]
  , prod = \tokens -> case tokens of
      (Token Distance dd:_) ->
        Just . Token Distance $ withUnit TDistance.Foot dd
      _ -> Nothing
  }

ruleDistMeters :: Rule
ruleDistMeters = Rule
  { name = "<dist> meters"
  , pattern =
    [ dimension Distance
    , regex "m|(\xbbf8|\xba54|\xb9e4)\xd130"
    ]
  , prod = \tokens -> case tokens of
      (Token Distance dd:_) ->
        Just . Token Distance $ withUnit TDistance.Metre dd
      _ -> Nothing
  }

ruleLatentDistFeet :: Rule
ruleLatentDistFeet = Rule
  { name = "<latent dist> feet"
  , pattern =
    [ dimension Distance
    , regex "('|f(oo|ee)?ts?)|\xd53c\xd2b8"
    ]
  , prod = \tokens -> case tokens of
      (Token Distance dd:_) ->
        Just . Token Distance $ withUnit TDistance.Foot dd
      _ -> Nothing
  }

ruleLatentDistKm :: Rule
ruleLatentDistKm = Rule
  { name = "<latent dist> km"
  , pattern =
    [ dimension Distance
    , regex "km|(\xd0ac|\xd0a4)\xb85c((\xbbf8|\xba54)\xd130)?"
    ]
  , prod = \tokens -> case tokens of
      (Token Distance dd:_) ->
        Just . Token Distance $ withUnit TDistance.Kilometre dd
      _ -> Nothing
  }

ruleHalf :: Rule
ruleHalf = Rule
  { name = "half"
  , pattern =
    [ regex "\xbc18"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData {TNumeral.value = v}:_) ->
        Just . Token Distance $ distance v
      _ -> Nothing
  }

ruleDistMiles :: Rule
ruleDistMiles = Rule
  { name = "<dist> miles"
  , pattern =
    [ dimension Distance
    , regex "miles?|\xb9c8\xc77c(\xc988)?"
    ]
  , prod = \tokens -> case tokens of
      (Token Distance dd:_) ->
        Just . Token Distance $ withUnit TDistance.Mile dd
      _ -> Nothing
  }

ruleLatentDistInch :: Rule
ruleLatentDistInch = Rule
  { name = "<latent dist> inch"
  , pattern =
    [ dimension Distance
    , regex "(''|inch(es)?)|\xc778\xce58"
    ]
  , prod = \tokens -> case tokens of
      (Token Distance dd:_) ->
        Just . Token Distance $ withUnit TDistance.Inch dd
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleDistCentimeters
  , ruleDistMeters
  , ruleDistMiles
  , ruleHalf
  , ruleLatentDistFeet
  , ruleLatentDistFeetAndLatentDistInch
  , ruleLatentDistInch
  , ruleLatentDistKm
  , ruleLatentDistYard
  ]
