-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Volume.ZH.Rules
  ( rules ) where

import Data.Text (Text)
import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Types
import Duckling.Volume.Helpers
import Duckling.Numeral.Helpers (isPositive)
import qualified Duckling.Volume.Types as TVolume
import qualified Duckling.Numeral.Types as TNumeral

volumes :: [(Text, String, TVolume.Unit)]
volumes = [ ("<latent vol> ml", "cc|ml|毫升", TVolume.Millilitre)
          , ("<vol> liters", "l|L|公升|升", TVolume.Litre)
          , ("<latent vol> gallon", "加侖", TVolume.Gallon)
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

ruleUnitTeaspoon :: Rule
ruleUnitTeaspoon = Rule
  { name = "<numeral> teaspoon"
  , pattern =
    [ Predicate isPositive
    , regex "茶匙"
    ]
    , prod = \case
      (Token Numeral TNumeral.NumeralData{TNumeral.value = v}:_) ->
        Just . Token Volume $ volume TVolume.Millilitre (5*v)
      _ -> Nothing
  }

ruleUnitSoupspoon :: Rule
ruleUnitSoupspoon = Rule
  { name = "<numeral> soupspoon"
  , pattern =
    [ Predicate isPositive
    , regex "湯匙"
    ]
    , prod = \case
      (Token Numeral TNumeral.NumeralData{TNumeral.value = v}:_) ->
        Just . Token Volume $ volume TVolume.Millilitre (15*v)
      _ -> Nothing
  }

rulePrecision :: Rule
rulePrecision = Rule
  { name = "about <volume>"
  , pattern =
    [ regex "\\~|大約|約"
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
    [ Predicate isPositive
    , regex "-|~|至|到"
    , Predicate isSimpleVolume
    ]
  , prod = \case
      (Token Numeral TNumeral.NumeralData{TNumeral.value = from}:
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
    [ Predicate isSimpleVolume
    , regex "-|~|至|到"
    , Predicate isSimpleVolume
    ]
  , prod = \case
      (Token Volume TVolume.VolumeData{TVolume.value = Just from
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
    [ regex "最多"
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

ruleIntervalMax2 :: Rule
ruleIntervalMax2 = Rule
  { name = "<volume> or below"
  , pattern =
    [ Predicate isSimpleVolume
    , regex "(或)?以下"
    ]
  , prod = \case
      (Token Volume TVolume.VolumeData{TVolume.value = Just to
                                  , TVolume.unit = Just u}:
       _) ->
        Just . Token Volume . withMax to $ unitOnly u
      _ -> Nothing
  }

ruleIntervalMin :: Rule
ruleIntervalMin = Rule
  { name = "more than <volume>"
  , pattern =
      [ regex "至少|最少|起碼"
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

ruleIntervalMin2 :: Rule
ruleIntervalMin2 = Rule
  { name = "<volume> or above"
  , pattern =
      [ Predicate isSimpleVolume
      , regex "(或)?以上"
      ]
    , prod = \case
        (Token Volume TVolume.VolumeData{TVolume.value = Just from
                                    , TVolume.unit = Just u}:
         _) ->
          Just . Token Volume . withMin from $ unitOnly u
        _ -> Nothing
    }


rules :: [Rule]
rules = [ ruleUnitTeaspoon
        , ruleUnitSoupspoon
        , rulePrecision
        , ruleIntervalBetweenNumeral
        , ruleIntervalBetween
        , ruleIntervalMax
        , ruleIntervalMax2
        , ruleIntervalMin
        , ruleIntervalMin2
        ]
        ++ rulesVolumes
