-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Distance.KM.Rules
  ( rules
  ) where

import Data.String
import Data.Text (Text)
import Prelude

import Duckling.Dimensions.Types
import Duckling.Distance.Helpers
import Duckling.Distance.Types (DistanceData(..))
import Duckling.Numeral.Helpers
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Types
import qualified Duckling.Distance.Types as TDistance
import qualified Duckling.Numeral.Types as TNumeral

distances :: [(Text, String, TDistance.Unit)]
distances = [ -- Metric
            ("km", "km|គីឡូ(ម៉ែត្រ)?", TDistance.Kilometre)
            , ("meters", "m|ម៉ែត្រ", TDistance.Metre)
            , ("centimeters", "cm|សង់ទីម៉ែត្រ", TDistance.Centimetre)
            , ("millimeters", "mm|មីលីម៉ែត្រ", TDistance.Millimetre)
            ]

rulePrecision :: Rule
rulePrecision = Rule
  { name = "about|exactly <dist>"
  , pattern =
    [ regex "\\~|ប្រហែល"
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
    [ regex "ចន្លោះ(ពី)?|ចាប់ពី"
    , Predicate isPositive
    , regex "និង|ដល់"
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
    [ regex "ចន្លោះ(ពី)?|ចាប់ពី"
    , Predicate isSimpleDistance
    , regex "និង|ដល់"
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
  { name = "Max Rule"
  , pattern =
    [ regex "ក្រោម|តិចជាង|មិនដល់|យ៉ាងច្រើន|មិនលើស"
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
  { name = "Min Rule"
  , pattern =
    [ regex "លើស(ពី)?|មិនតិចជាង|លើ|ច្រើនជាង|យ៉ាងតិច|យ៉ាងហោច"
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
