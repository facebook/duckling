-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.TimeGrain.BG.Rules
  ( rules
  ) where

import Data.String
import Data.Text (Text)
import Prelude

import Duckling.Dimensions.Types
import Duckling.TimeGrain.Types
import Duckling.Types

grains :: [(Text, String, Grain)]
grains = [ ("second (grain) ", "сек(унд(а|и))?",               Second)
         , ("minute (grain)" , "мин(ут(а|и))?",                Minute)
         , ("hour (grain)"   , "ч(ас(ове(те)?|а|ът)?)?",       Hour)
         , ("day (grain)"    , "ден(а|я(т)?)?|дни(те)?",       Day)
         , ("week (grain)"   , "седмица(та)?|седмици(те)?",    Week)
         , ("month (grain)"  , "месец(и(те)?|а|ът)?",          Month)
         , ("quarter (grain)", "тримесечи(я|е(то|та)?)",       Quarter)
         , ("year (grain)"   , "г(од(ин(а(та)?|и(те)?))?)?",   Year)
         ]

rules :: [Rule]
rules = map go grains
  where
    go (name, regexPattern, grain) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> Just $ Token TimeGrain grain
      }
