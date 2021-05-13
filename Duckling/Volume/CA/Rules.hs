-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Volume.CA.Rules
  ( rules ) where

import Data.String
import Data.Text (Text)
import Prelude

import Duckling.Dimensions.Types
import Duckling.Types
import Duckling.Volume.Helpers
import qualified Duckling.Volume.Types as TVolume

volumes :: [(Text, String, TVolume.Unit)]
volumes = [ ("<latent vol> ml"    , "m(l|ililitres?)", TVolume.Millilitre)
          , ("<vol> hectoliters"  , "(hectolitres?)" , TVolume.Hectolitre)
          , ("<vol> liters"       , "l(itres?)?"     , TVolume.Litre)
          , ("<latent vol> gallon", "gal(ons|ó)"     , TVolume.Gallon)
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
      , prod = \_ -> Just $ Token Volume $ unitOnly u
      }

fractions :: [(Text, String, Double)]
fractions = [ ("half", "mig", 1/2)
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
          Just $ Token Volume $ volume u f
        _ -> Nothing
      }

rules :: [Rule]
rules =
     rulesVolumes
  ++ rulesFractionalVolume
