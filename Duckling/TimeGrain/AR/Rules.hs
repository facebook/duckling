-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.TimeGrain.AR.Rules
  ( rules
  ) where

import Data.String
import Data.Text (Text)
import Prelude

import Duckling.Dimensions.Types
import Duckling.Types
import qualified Duckling.TimeGrain.Types as TG

grains :: [(Text, String, TG.Grain)]
grains = [ ("second (grain) ", "(ثاني(ة|ه)?|ثواني|لحظ(ة|ه|ات))"   , TG.Second)
         , ("minute (grain)" , "دق(يق(ة|ه)|ائق)"                  , TG.Minute)
         , ("hour (grain)"   , "ساع(ة|ه|ات)"                      , TG.Hour)
         , ("day (grain)"    , "يوم|(ا|أ)يام"                     , TG.Day)
         , ("week (grain)"   , "(ا|أ|إ)س(بوع|ابيع)"               , TG.Week)
         , ("month (grain)"  , "شهر|(ا|أ|إ)شهر"                   , TG.Month)
         , ("quarter (grain)", "(ربع(ين|ان)|[أا]رباع)(سنة|عام)?"  , TG.Quarter)
         , ("year (grain)"   , "سن(ة|ين)|عام"                     , TG.Year)
         ]

rules :: [Rule]
rules = map go grains
  where
    go (name, regexPattern, grain) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> Just $ Token TimeGrain grain
      }
