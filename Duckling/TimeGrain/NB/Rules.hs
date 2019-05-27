-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.TimeGrain.NB.Rules
  ( rules ) where

import Data.Text (Text)
import Prelude
import Data.String

import Duckling.Dimensions.Types
import qualified Duckling.TimeGrain.Types as TG
import Duckling.Types

grains :: [(Text, String, TG.Grain)]
grains = [ ("second (grain)", "sek(und(er)?)?", TG.Second)
         , ("minute (grain)", "min(utt(er)?)?", TG.Minute)
         , ("hour (grain)", "t(ime(r)?)?", TG.Hour)
         , ("day (grain)", "dag(er)?", TG.Day)
         , ("week (grain)", "uke(r|n)?", TG.Week)
         , ("month (grain)", "måned(er)?", TG.Month)
         , ("quarter (grain)", "kvart(al|er)(et)?", TG.Quarter)
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
