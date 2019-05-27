-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.TimeGrain.DA.Rules
  ( rules ) where

import Data.Text (Text)
import Prelude
import Data.String

import Duckling.Dimensions.Types
import qualified Duckling.TimeGrain.Types as TG
import Duckling.Types

grains :: [(Text, String, TG.Grain)]
grains = [ ("second (grain)", "sekund(er)?", TG.Second)
         , ("minute (grain)", "minut(ter)?", TG.Minute)
         , ("hour (grain)", "t(imer?)?", TG.Hour)
         , ("day (grain)", "dag(e)?", TG.Day)
         , ("week (grain)", "uger?", TG.Week)
         , ("month (grain)", "måned(er)?", TG.Month)
         , ("quarter (grain)", "kvartal(er)?", TG.Quarter)
         , ("year (grain)", "år", TG.Year)
         ]

rules :: [Rule]
rules = map go grains
  where
    go (name, regexPattern, grain) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> Just $ Token TimeGrain grain
      }
