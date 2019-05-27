-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Volume.DE.Rules
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
volumes = [ ("<latent vol> ml", "m(l|illiliter[ns]?)", TVolume.Millilitre)
          , ("<vol> hectoliters", "hektoliter[ns]?", TVolume.Hectolitre)
          , ("<vol> liters", "l(iter[ns]?)?", TVolume.Litre)
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
fractions = [ ("one","ein(e[ns])?", 1)
            , ("half", "(ein(e[ns])? )?halb(e[rn])? ", 1/2)
            , ("third", "(ein(e[ns])? )?dritttel ", 1/3)
            , ("fourth", "(ein(e[ns])? )?viertel ", 1/4)
            , ("fifth", "(ein(e[ns])? )?fünftel ", 1/5)
            , ("tenth", "(ein(e[ns])? )?zehntel ", 1/10)
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
    [ regex "\\~|(ganz )?genau|präzise|(in )?etwa|ungefähr|um( die)?|fast"
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
    [ regex "zwischen|von"
    , Predicate isPositive
    , regex "und|bis( zu)?"
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
    [ regex "zwischen|von"
    , Predicate isSimpleVolume
    , regex "bis( zu)?|und"
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
    [ regex "unter|weniger( als)?|höchstens|nicht mehr als"
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
      [ regex "über|mindestens|wenigstens|mehr als|größer( als)?"
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
