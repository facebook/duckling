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
import Duckling.Distance.Types (DistanceData(..))
import Duckling.Numeral.Helpers
import Duckling.Numeral.Types
import Duckling.Regex.Types
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
    , regex "''|\"|inch(es)?|英寸|英吋|吋"
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
    , regex "''|\"|inch(es)?|英寸|英吋|吋"
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

ruleDistOneMeterAnd :: Rule
ruleDistOneMeterAnd = Rule
  { name = "one meter and <dist>"
  , pattern =
    [ regex "米"
    , Predicate isPositive
    ]
  , prod = \case
      (_:Token Numeral NumeralData{TNumeral.value = v}:_) ->
        Just . Token Distance $ withUnit TDistance.Metre (distance (1 + v/10))
      _ -> Nothing
  }

ruleDistMetersAnd :: Rule
ruleDistMetersAnd = Rule
  { name = "<dist> meters and <dist>"
  , pattern =
    [ Predicate isPositive
    , regex "米"
    , Predicate isPositive
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = v1}:_:
        Token Numeral NumeralData{TNumeral.value = v2}:_) ->
        Just . Token Distance $ withUnit TDistance.Metre (distance (v1 + v2/10))
      _ -> Nothing
  }

ruleIntervalNumeralDash :: Rule
ruleIntervalNumeralDash = Rule
  { name = "<numeral> - <dist>"
  , pattern =
    [ Predicate isPositive
    , regex "-|~|到|至"
    , Predicate isSimpleDistance
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = from}:
       _:
       Token Distance DistanceData{TDistance.value = Just to,
       TDistance.unit = Just u}:_) | from < to ->
         Just $ Token Distance $ withInterval (from, to) $ unitOnly u
      _ -> Nothing
  }

ruleIntervalDash :: Rule
ruleIntervalDash = Rule
  { name = "<dist> - <dist>"
  , pattern =
    [ Predicate isSimpleDistance
    , regex "-|~|到|至"
    , Predicate isSimpleDistance
    ]
  , prod = \case
      (Token Distance DistanceData{TDistance.value = Just from,
                  TDistance.unit = Just u1}:
       _:
       Token Distance DistanceData{TDistance.value = Just to, TDistance.unit = Just u2}:
       _) | from < to && u1 == u2 ->
         Just $ Token Distance $ withInterval (from, to) $ unitOnly u1
      _ -> Nothing
  }

ruleIntervalBound :: Rule
ruleIntervalBound = Rule
  { name = "under/less/lower/no more than <amount-of-money> (最多|至少|最少)"
  , pattern =
    [ regex "(最多|至少|最少|起碼)"
    , Predicate isSimpleDistance
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):
       Token Distance DistanceData{TDistance.value = Just to,
                  TDistance.unit = Just u}:
       _) -> case match of
        "最多" -> Just $ Token Distance $ withMax to $ unitOnly u
        "最少" -> Just $ Token Distance $ withMin to $ unitOnly u
        "至少" -> Just $ Token Distance $ withMin to $ unitOnly u
        "起碼" -> Just $ Token Distance $ withMin to $ unitOnly u
        _ -> Nothing
      _ -> Nothing
  }

ruleIntervalBound2 :: Rule
ruleIntervalBound2 = Rule
  { name = "under/less/lower/no more than <amount-of-money> (以下|以上)"
  , pattern =
    [ Predicate isSimpleDistance
    , regex "(以下|以上)"
    ]
  , prod = \case
      (Token Distance DistanceData{TDistance.value = Just to,
                  TDistance.unit = Just u}:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> case match of
        "以下" -> Just $ Token Distance $ withMax to $ unitOnly u
        "以上" -> Just $ Token Distance $ withMin to $ unitOnly u
        _ -> Nothing
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
  , ruleDistOneMeterAnd
  , ruleDistMetersAnd
  , ruleDistOneMeterAnd
  , ruleDistMetersAnd
  , ruleIntervalNumeralDash
  , ruleIntervalDash
  , ruleIntervalBound
  , ruleIntervalBound2
  ]
