-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.TimeGrain.EL.Rules
  ( rules ) where

import Data.Text (Text)
import Data.String
import Prelude

import Duckling.Dimensions.Types
import qualified Duckling.TimeGrain.Types as TG
import Duckling.Types

grains :: [(Text, String, TG.Grain)]
grains =
  [ ("second (grain) ", "δε[υύ]τερ([οό]λ[εέ]πτ)?(ου?|α|ων)", TG.Second)
  , ("minute (grain)" , "λεπτ(o|όν?|ού|ά|ών)"              , TG.Minute)
  , ("hour (grain)"   , "[ωώ](ρ(ας?|ες|ών))?"              , TG.Hour)
  , ("day (grain)"    , "η?μέρ(ας?|ες|ών)"                 , TG.Day)
  , ("week (grain)"   , "ε?βδομάδ(ας?ν?|ες|ων)"            , TG.Week)
  , ("month (grain)"  , "μήν(ας?|ες|ών)"                   , TG.Month)
  , ("quarter (grain)", "τρ[ιί]μ[ηή]ν(ου?|α|ων)"           , TG.Quarter)
  , ("year (grain)"   , "έτ(ου?ς|η|ών)|χρ[οό]ν(ο[ιςυ]?|ι([αά]|ές)|ι?ών)"
                      , TG.Year)
  ]

rules :: [Rule]
rules = map go grains
  where
    go (name, regexPattern, grain) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> Just $ Token TimeGrain grain
      }
