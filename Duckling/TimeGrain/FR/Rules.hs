-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.TimeGrain.FR.Rules
  ( rules ) where

import Data.Text (Text)
import Prelude
import Data.String

import Duckling.Dimensions.Types
import qualified Duckling.TimeGrain.Types as TG
import Duckling.Types

grains :: [(Text, String, TG.Grain)]
grains = [ ("seconde (grain)", "sec(onde)?s?", TG.Second)
         , ("minute (grain)", "min(ute)?s?", TG.Minute)
         , ("heure (grain)", "heures?", TG.Hour)
         , ("jour (grain)", "jour(n(e|é)e?)?s?", TG.Day)
         , ("semaine (grain)", "semaines?", TG.Week)
         , ("mois (grain)", "mois", TG.Month)
         , ("trimestre (grain)", "trimestres?", TG.Quarter)
         , ("année (grain)", "an(n(e|é)e?)?s?", TG.Year)
         ]

rules :: [Rule]
rules = map go grains
  where
    go (name, regexPattern, grain) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> Just $ Token TimeGrain grain
      }
