-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.TimeGrain.DE.Rules
  ( rules ) where

import Data.Text (Text)
import Prelude
import Data.String

import Duckling.Dimensions.Types
import qualified Duckling.TimeGrain.Types as TG
import Duckling.Types

grains :: [(Text, String, TG.Grain)]
grains = [ ("second (grain)", "sekunden?", TG.Second)
         , ("minute (grain)", "minuten?", TG.Minute)
         , ("hour (grain)", "stunden?", TG.Hour)
         , ("day (grain)", "tage?n?", TG.Day)
         , ("week (grain)", "wochen?", TG.Week)
         , ("month (grain)", "monate?n?", TG.Month)
         , ("quarter (grain)", "quartale?", TG.Quarter)
         , ("year (grain)", "jahre?n?", TG.Year)
         ]

rules :: [Rule]
rules = map go grains
  where
    go (name, regexPattern, grain) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> Just $ Token TimeGrain grain
      }
