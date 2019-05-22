-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Volume.GA.Rules
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
volumes = [ ("<latent vol> ml", "m(l\\.?|h?illil(í|i)t(ea|i)r)"
            , TVolume.Millilitre)
          , ("<vol> hectoliters" , "heictil(í|i)t(ea|i)r"
            , TVolume.Hectolitre)
          , ("<vol> liters", "(l(í|i)t(ea|i)r|l\\.?)"
            , TVolume.Litre)
          , ("<latent vol> gallon", "n?gh?al(ú|u)i?n"
            , TVolume.Gallon)
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

ruleKiloliter :: Rule
ruleKiloliter = Rule
  { name = "kiloliter"
  , pattern =
    [ Predicate isPositive
    , regex "(kl\\.?|g?ch?illil(í|i)t(ea|i)r)"
    ]
  , prod = \case
    (Token Numeral TNumeral.NumeralData{TNumeral.value = v}:
     _:_) ->
      Just . Token Volume . withUnit TVolume.Hectolitre $ valueOnly (10.0 * v)
    _ -> Nothing
  }

rules :: [Rule]
rules = [ ruleKiloliter
        ]
        ++ rulesVolumes
