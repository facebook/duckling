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
import Data.Text (Text)
import Prelude

import Duckling.Dimensions.Types
import Duckling.Distance.Helpers
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Types
import qualified Duckling.Distance.Types as TDistance
import qualified Duckling.Numeral.Types as TNumeral

distances :: [(Text, String, TDistance.Unit)]
distances = [ -- Imperial
              ("miles", "miles?|英(里|裏)", TDistance.Mile)
            , ("yard", "碼", TDistance.Yard)
            , ("feet", "'|f(oo|ee)?ts?|英尺|呎", TDistance.Foot)
            , ("inch", "''|inch(es)?|英寸|英吋|吋|\"", TDistance.Inch)
              -- Metric
            , ("km", "km|千米|公(里|裏)", TDistance.Kilometre)
            , ("meters", "m|米|公尺", TDistance.Metre)
            , ("centimeters", "cm|厘米|公分", TDistance.Centimetre)
            , ("millimeters", "mm|毫米", TDistance.Millimetre)
            ]

ruleDistances :: [Rule]
ruleDistances = map go distances
  where
    go :: (Text, String, TDistance.Unit) -> Rule
    go (name, regexPattern, u) = Rule
      { name = name
      , pattern = [ dimension Distance, regex regexPattern ]
      , prod = \case
          (Token Distance dd:_) -> Just . Token Distance $ withUnit u dd
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
      (_:Token Numeral NumeralData{TNumeral.value = Just v}:_) ->
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
      (Token Numeral NumeralData{TNumeral.value = Just v1}:_:
        Token Numeral NumeralData{TNumeral.value = Just v2}:_) ->
        Just . Token Distance $ withUnit TDistance.Metre (distance (v1 + v2/10)) 
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
      (Token Distance DistanceData{TDistance.value = Just f}:_:
        Token Distance DistanceData{TDistance.value = Just i}:_) ->
        Just . Token Distance $ withUnit TDistance.Inch (distance(f*12 + i))
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
      (Token Numeral NumeralData{TNumeral.value = Just from}:
       _:
       Token Distance DistanceData{TDistance.value = Just to,
       TDistance.unit = Just u}:_) | from < to ->
         Just . Token Distance . withInterval (from, to) $ unitOnly u
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
       Token Distance DistanceData{TDistance.value = Just to,
                  TDistance.unit = Just u2}:
       _) | from < to && u1 == u2 ->
        Just . Token Distance . withInterval (from, to) $ unitOnly u1
      _ -> Nothing
  }

ruleIntervalFromNumeral :: [Rule]
ruleIntervalFromNumeral = map go distances
  where
    go :: (Text, String, TDistance.Unit) -> Rule
    go (name, regexPattern, u) = Rule
      { name = name
      , pattern = [ Predicate isNumeralInterval, regex regexPattern ]
      , prod = \case
          (Token Numeral NumeralData{TNumeral.minValue = Just from, TNumeral.maxValue = Just to}:
            _) -> Just . Token Distance $ withInterval (from, to) $ unitOnly u
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
        "最多" -> Just . Token Distance . withMax to $ unitOnly u
        "最少" -> Just . Token Distance . withMin to $ unitOnly u
        "至少" -> Just . Token Distance . withMin to $ unitOnly u
        "起碼" -> Just . Token Distance . withMin to $ unitOnly u
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
        "以下" -> Just . Token Distance . withMax to $ unitOnly u
        "以上" -> Just . Token Distance . withMin to $ unitOnly u
        _ -> Nothing
      _ -> Nothing
  }

rulePrecision :: Rule
rulePrecision = Rule
  { name = "about <distance>"
  , pattern =
    [ dimension Distance
    , regex "左右"
    ]
  , prod = \case
      (token:_) -> Just token
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleDistFeetAndDistInch
  , ruleDistOneMeterAnd
  , ruleDistMetersAnd
  , ruleIntervalNumeralDash
  , ruleIntervalDash
  , ruleIntervalBound
  , ruleIntervalBound2
  , rulePrecision
  ]
  ++ ruleDistances
  ++ ruleIntervalFromNumeral
