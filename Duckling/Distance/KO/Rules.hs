-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


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
    , regex "y(ar)?ds?|야드"
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
    , regex "cm|센(티|치)((미|메)터)?"
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
    , regex "('|f(oo|ee)?ts?)|피트"
    , dimension Distance
    , regex "(''|inch(es)?)|인치"
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
    , regex "m|(미|메|매)터"
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
    , regex "('|f(oo|ee)?ts?)|피트"
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
    , regex "km|(킬|키)로((미|메)터)?"
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
    [ regex "반"
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
    , regex "miles?|마일(즈)?"
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
    , regex "(''|inch(es)?)|인치"
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
