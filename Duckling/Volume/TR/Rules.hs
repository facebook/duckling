-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Volume.TR.Rules
  ( rules ) where

import Data.String
import Data.Text (Text)
import Prelude

import Duckling.Dimensions.Types
import Duckling.Types
import Duckling.Regex.Types
import Duckling.Volume.Helpers
import Duckling.Numeral.Helpers (isPositive)
import qualified Duckling.Volume.Types as TVolume
import qualified Duckling.Numeral.Types as TNumeral

volumes :: [(Text, String, TVolume.Unit)]
volumes = [ ("<latent vol> ml"    , "m(l|ililitre)" , TVolume.Millilitre)
          , ("<vol> hectoliters"  , "(hektolitre)"  , TVolume.Hectolitre)
          , ("<vol> liters"       , "l(t|itre)"     , TVolume.Litre)
          , ("<latent vol> gallon", "gal(l?on?)?"   , TVolume.Gallon)
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
fractions = [ ("half", "yar\305m", 1/2)
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

rules :: [Rule]
rules =
  [
  ]
  ++ rulesVolumes
  ++ rulesFractionalVolume
