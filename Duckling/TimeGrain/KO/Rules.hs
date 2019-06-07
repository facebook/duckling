-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.TimeGrain.KO.Rules
  ( rules ) where

import Data.Text (Text)
import Prelude
import Data.String

import Duckling.Dimensions.Types
import qualified Duckling.TimeGrain.Types as TG
import Duckling.Types

grains :: [(Text, String, TG.Grain)]
grains = [ ("second (grain)", "초", TG.Second)
         , ("minute (grain)", "분", TG.Minute)
         , ("hour (grain)", "시(간)?", TG.Hour)
         , ("day (grain)", "날|일(간|동안)?", TG.Day)
         , ("week (grain)", "주(간|동안|일)?", TG.Week)
         , ("month (grain)", "(달)(간|동안)?", TG.Month)
         , ("quarter (grain)", "분기(간|동안)?", TG.Quarter)
         , ("year (grain)", "해|연간|년(간|동안)?", TG.Year)
         ]

rules :: [Rule]
rules = map go grains
  where
    go (name, regexPattern, grain) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> Just $ Token TimeGrain grain
      }
