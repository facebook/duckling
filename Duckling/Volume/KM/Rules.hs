-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Volume.KM.Rules
  ( rules ) where

import Data.Text (Text)
import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Types
import Duckling.Regex.Types
import Duckling.Volume.Helpers
import Duckling.Numeral.Helpers (isPositive)
import qualified Duckling.Volume.Types as TVolume
import qualified Duckling.Numeral.Types as TNumeral

volumes :: [(Text, String, TVolume.Unit)]
volumes = [ ("<latent vol> ml", "ml|មីលីលីត្រ", TVolume.Millilitre)
          , ("<vol> liters", "l|លីត្រ", TVolume.Litre)
          ]

rulesVolumes :: [Rule]
rulesVolumes = map go volumes
  where
    go :: (Text, String, TVolume.Unit) -> Rule
    go (name, regexPattern, u) = Rule
      { name = name
      , pattern =
        [ regex regexPattern
        ]
      , prod = \_ -> Just . Token Volume $ unitOnly u
      }

fractions :: [(Text, String, Double)]
fractions = [ ("half", "កន្លះ|១\\/២", 1/2)
            , ("third", "មួយភាគបី|១\\/៣", 1/3)
            , ("fourth", "មួយភាគបួន|១\\/៤", 1/4)
            , ("fifth", "មួយភាគប្រាំ|១\\/៥", 1/5)
            ]

rulesFractionalVolume :: [Rule]
rulesFractionalVolume = map go fractions
  where
    go :: (Text, String, Double) -> Rule
    go (name, regexPattern, f) = Rule
      { name = name
      , pattern =
        [ regex regexPattern
        , Predicate isUnitOnly
        ]
      , prod = \case
        (_:
         Token Volume TVolume.VolumeData{TVolume.unit = Just u}:
         _) ->
          Just . Token Volume $ volume u f
        _ -> Nothing
      }

rulePrecision :: Rule
rulePrecision = Rule
  { name = "about <volume>"
  , pattern =
    [ regex "\\~|ប្រហែល"
    , dimension Volume
    ]
    , prod = \case
      (_:token:_) -> Just token
      _ -> Nothing
  }

ruleIntervalBetweenNumeral :: Rule
ruleIntervalBetweenNumeral = Rule
  { name = "between|from <numeral> and|to <volume>"
  , pattern =
    [ regex "ចន្លោះ(ពី)?|ចាប់ពី"
    , Predicate isPositive
    , regex "និង|ដល់"
    , Predicate isSimpleVolume
    ]
  , prod = \case
      (_:
       Token Numeral TNumeral.NumeralData{TNumeral.value = from}:
       _:
       Token Volume TVolume.VolumeData{TVolume.value = Just to
                                  , TVolume.unit = Just u}:
       _) | from < to ->
        Just . Token Volume . withInterval (from, to) $ unitOnly u
      _ -> Nothing
  }

ruleIntervalBetween :: Rule
ruleIntervalBetween = Rule
  { name = "between|from <volume> to|and <volume>"
  , pattern =
    [ regex "ចន្លោះ(ពី)?|ចាប់ពី"
    , Predicate isSimpleVolume
    , regex "និង|ដល់"
    , Predicate isSimpleVolume
    ]
  , prod = \case
      (_:
       Token Volume TVolume.VolumeData{TVolume.value = Just from
                                  , TVolume.unit = Just u1}:
       _:
       Token Volume TVolume.VolumeData{TVolume.value = Just to
                                  , TVolume.unit = Just u2}:
       _) | from < to && u1 == u2 ->
        Just . Token Volume . withInterval (from, to) $ unitOnly u1
      _ -> Nothing
  }

ruleIntervalMax :: Rule
ruleIntervalMax = Rule
  { name = "at most <volume>"
  , pattern =
    [ regex "ក្រោម|តិចជាង|មិនដល់|យ៉ាងច្រើន|មិនលើស"
    , Predicate isSimpleVolume
    ]
  , prod = \case
      (_:
       Token Volume TVolume.VolumeData{TVolume.value = Just to
                                  , TVolume.unit = Just u}:
       _) ->
        Just . Token Volume . withMax to $ unitOnly u
      _ -> Nothing
  }

ruleIntervalMin :: Rule
ruleIntervalMin = Rule
  { name = "more than <volume>"
  , pattern =
      [ regex "លើស(ពី)?|មិនតិចជាង|លើ|ច្រើនជាង|យ៉ាងតិច|យ៉ាងហោច"
      , Predicate isSimpleVolume
      ]
    , prod = \case
        (_:
         Token Volume TVolume.VolumeData{TVolume.value = Just from
                                    , TVolume.unit = Just u}:
         _) ->
          Just . Token Volume . withMin from $ unitOnly u
        _ -> Nothing
    }

rules :: [Rule]
rules = [ rulePrecision
        , ruleIntervalBetweenNumeral
        , ruleIntervalBetween
        , ruleIntervalMax
        , ruleIntervalMin
        ]
        ++ rulesVolumes
        ++ rulesFractionalVolume
