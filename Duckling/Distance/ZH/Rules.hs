-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Distance.ZH.Rules
  ( rules ) where

import Data.String
import Prelude

import Duckling.Dimensions.Types
import Duckling.Distance.Helpers
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Types
import qualified Duckling.Distance.Types as TDistance
import qualified Duckling.Numeral.Types as TNumeral

ruleDistCentimeters :: Rule
ruleDistCentimeters = Rule
  { name = "<dist> centimeters"
  , pattern =
    [ dimension Distance
    , regex "cm|厘米|公分"
    ]
  , prod = \case
      (Token Distance dd:_) ->
        Just . Token Distance $ withUnit TDistance.Centimetre dd
      _ -> Nothing
  }

ruleDistMeters :: Rule
ruleDistMeters = Rule
  { name = "<dist> meters"
  , pattern =
    [ dimension Distance
    , regex "m|米|公尺"
    ]
  , prod = \case
      (Token Distance dd:_) ->
        Just . Token Distance $ withUnit TDistance.Metre dd
      _ -> Nothing
  }

ruleDistKm :: Rule
ruleDistKm = Rule
  { name = "<dist> km"
  , pattern =
    [ dimension Distance
    , regex "km|千米|公(里|裏)"
    ]
  , prod = \case
      (Token Distance dd:_) ->
        Just . Token Distance $ withUnit TDistance.Kilometre dd
      _ -> Nothing
  }


ruleDistFeetAndDistInch :: Rule
ruleDistFeetAndDistInch = Rule
  { name = "<dist> feet and <dist> inch "
  , pattern =
    [ dimension Distance
    , regex "'|f(oo|ee)?ts?|英尺|呎"
    , dimension Distance
    , regex "''|inch(es)?|英寸|英吋|吋"
    ]
  , prod = \case
      (Token Distance dd:_) ->
        Just . Token Distance $ withUnit TDistance.Foot dd
      _ -> Nothing
  }

ruleDistInch :: Rule
ruleDistInch = Rule
  { name = "<dist> inch"
  , pattern =
    [ dimension Distance
    , regex "''|inch(es)?|英寸|英吋|吋"
    ]
  , prod = \case
      (Token Distance dd:_) ->
        Just . Token Distance $ withUnit TDistance.Inch dd
      _ -> Nothing
  }

ruleDistFeet :: Rule
ruleDistFeet = Rule
  { name = "<dist> feet"
  , pattern =
    [ dimension Distance
    , regex "'|f(oo|ee)?ts?|英尺|呎"
    ]
  , prod = \case
      (Token Distance dd:_) ->
        Just . Token Distance $ withUnit TDistance.Foot dd
      _ -> Nothing
  }

ruleDistMiles :: Rule
ruleDistMiles = Rule
  { name = "<dist> miles"
  , pattern =
    [ dimension Distance
    , regex "miles?|英(里|裏)"
    ]
  , prod = \case
      (Token Distance dd:_) ->
        Just . Token Distance $ withUnit TDistance.Mile dd
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleDistCentimeters
  , ruleDistFeet
  , ruleDistFeetAndDistInch
  , ruleDistInch
  , ruleDistKm
  , ruleDistMeters
  , ruleDistMiles
  ]
