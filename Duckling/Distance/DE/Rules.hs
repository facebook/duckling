-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Duckling.Distance.DE.Rules (rules) where

import Data.String
import Data.Text (Text)
import Prelude

import Duckling.Dimensions.Types
import Duckling.Distance.Helpers
import Duckling.Distance.Types (DistanceData(..))
import qualified Duckling.Distance.Types as TDistance
import Duckling.Numeral.Helpers
import Duckling.Numeral.Types (NumeralData(..))
import qualified Duckling.Numeral.Types as TNumeral
import Duckling.Types

distances :: [(Text, String, TDistance.Unit)]
distances =
  [ -- Imperial
    ("miles", "meilen?", TDistance.Mile)
  , ("inch", "(\"|''|zoll)", TDistance.Inch)
              -- Metric
  , ("km", "k(ilo)?m(etern?)?", TDistance.Kilometre)
  , ("meters", "m(etern?)?", TDistance.Metre)
  , ("centimeters", "(cm|[zc]entimetern?)", TDistance.Centimetre)
  , ("millimeters", "(mm|millimetern?)", TDistance.Millimetre)
  ]

rulePrecision :: Rule
rulePrecision = Rule
  { name = "about|exactly <dist>"
  , pattern =
    [ regex "genau|exakt|präzise|ungefähr|(in )?etwa|nahe?( an)?|um( die)?|fast|rund|gut"
    , dimension Distance
    ]
  , prod = \case
      (_:token:_) -> Just token
      _ -> Nothing
  }

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

ruleIntervalBetweenNumeral :: Rule
ruleIntervalBetweenNumeral = Rule
  { name = "between|from <numeral> to|and <dist>"
  , pattern =
    [ regex "zwischen|von"
    , Predicate isPositive
    , regex "bis|und"
    , Predicate isSimpleDistance
    ]
  , prod = \case
      (_:
       Token Numeral NumeralData{TNumeral.value = from}:
       _:
       Token Distance DistanceData{TDistance.value = Just to, TDistance.unit = Just u}:
       _) | from < to ->
        Just . Token Distance . withInterval (from, to) $ unitOnly u
      _ -> Nothing
  }

ruleIntervalBetween :: Rule
ruleIntervalBetween = Rule
  { name = "between|from <dist> to|and <dist>"
  , pattern =
    [ regex "zwischen|von"
    , Predicate isSimpleDistance
    , regex "und|bis"
    , Predicate isSimpleDistance
    ]
  , prod = \case
      (_:
       Token Distance DistanceData{TDistance.value = Just from, TDistance.unit = Just u1}:
       _:
       Token Distance DistanceData{TDistance.value = Just to, TDistance.unit = Just u2}:
       _) | from < to && u1 == u2 ->
        Just . Token Distance . withInterval (from, to) $ unitOnly u1
      _ -> Nothing
  }

ruleIntervalNumeralDash :: Rule
ruleIntervalNumeralDash = Rule
  { name = "<numeral> - <dist>"
  , pattern =
    [ Predicate isPositive
    , regex "-"
    , Predicate isSimpleDistance
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = from}:
       _:
       Token Distance DistanceData{TDistance.value = Just to, TDistance.unit = Just u}:
       _) | from < to ->
         Just . Token Distance . withInterval (from, to) $ unitOnly u
      _ -> Nothing
  }

ruleIntervalDash :: Rule
ruleIntervalDash = Rule
  { name = "<dist> - <dist>"
  , pattern =
    [ Predicate isSimpleDistance
    , regex "-"
    , Predicate isSimpleDistance
    ]
  , prod = \case
      (Token Distance DistanceData{TDistance.value = Just from, TDistance.unit = Just u1}:
       _:
       Token Distance DistanceData{TDistance.value = Just to, TDistance.unit = Just u2}:
       _) | from < to && u1 == u2 ->
        Just . Token Distance . withInterval (from, to) $ unitOnly u1
      _ -> Nothing
  }

ruleIntervalMax :: Rule
ruleIntervalMax = Rule
  { name = "under/less/lower/no more than <dist>"
  , pattern =
    [ regex "unter|höchstens|maximal|(weniger|nicht mehr) als"
    , Predicate isSimpleDistance
    ]
  , prod = \case
      (_:
       Token Distance DistanceData{TDistance.value = Just to, TDistance.unit = Just u}:
       _) -> Just . Token Distance . withMax to $ unitOnly u
      _ -> Nothing
  }

ruleIntervalMin :: Rule
ruleIntervalMin = Rule
  { name = "over/above/at least/more than <dist>"
  , pattern =
    [ regex "über|(mehr|nicht weniger) als|mindestens|wenigstens|minimal"
    , Predicate isSimpleDistance
    ]
  , prod = \case
      (_:
       Token Distance DistanceData{TDistance.value = Just to, TDistance.unit = Just u}:
       _) -> Just . Token Distance . withMin to $ unitOnly u
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleIntervalBetweenNumeral
  , ruleIntervalBetween
  , ruleIntervalMax
  , ruleIntervalMin
  , ruleIntervalNumeralDash
  , ruleIntervalDash
  , rulePrecision
  ]
    ++ ruleDistances
