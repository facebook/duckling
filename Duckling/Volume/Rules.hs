-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Volume.Rules
  ( rules
  ) where

import Data.String
import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Types
import Duckling.Regex.Types
import Duckling.Volume.Helpers
import Duckling.Numeral.Helpers (isPositive)
import qualified Duckling.Volume.Types as TVolume
import qualified Duckling.Numeral.Types as TNumeral

ruleNumeralAsVolume :: Rule
ruleNumeralAsVolume = Rule
  { name = "number as volume"
  , pattern =
    [ Predicate isPositive
    ]
  , prod = \case
    (Token Numeral TNumeral.NumeralData {TNumeral.value = v}:
     _) ->
      Just . Token Volume $ valueOnly v
    _ -> Nothing
  }

ruleNumeralVolumes :: Rule
ruleNumeralVolumes = Rule
  { name = "<number> <volume>"
  , pattern =
    [ Predicate isPositive
    , Predicate isUnitOnly
    ]
  , prod = \case
    (Token Numeral TNumeral.NumeralData{TNumeral.value = v}:
     Token Volume TVolume.VolumeData{TVolume.unit = Just u}:
     _) ->
      Just . Token Volume $ volume u v
    _ -> Nothing
  }

ruleIntervalNumeralDash :: Rule
ruleIntervalNumeralDash = Rule
  { name = "<numeral> - <volume>"
  , pattern =
    [ Predicate isPositive
    , regex "\\-"
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

ruleIntervalDash :: Rule
ruleIntervalDash = Rule
  { name = "<volume> - <volume>"
  , pattern =
    [ Predicate isSimpleVolume
    , regex "\\-"
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

rules :: [Rule]
rules = [ ruleNumeralAsVolume
        , ruleNumeralVolumes
        , ruleIntervalNumeralDash
        , ruleIntervalDash
        ]
