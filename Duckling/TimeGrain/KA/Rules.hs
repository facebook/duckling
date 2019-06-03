-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.TimeGrain.KA.Rules
  ( rules
  ) where

import Data.String
import Data.Text (Text)
import Prelude

import Duckling.Dimensions.Types
import Duckling.TimeGrain.Types
import Duckling.Types

grains :: [(Text, String, Grain)]
grains = [ ("second (grain)" , "წამ(ით|ი|ში|ს)?", Second)
         , ("minute (grain)" , "წუთ(ით|ი|ში|ს)?", Minute)
         , ("hour (grain)" , "საათ(ით|ი|ში|ს)?", Hour)
         , ("day (grain)" , "დღით|დღის|დღე?(ში)?ს?", Day)
         , ("week (grain)" , "კვირ(აში)?(ას|ით|ის|ა)?", Week)
         , ("month (grain)" , "თვ(ეში|ით|ის|ეს|ს|ე)?", Month)
         , ("quarter (grain)" , "კვარტა?ლ(ით|ი|ში|ს)?", Quarter)
         , ("year (grain)" , "წლით|წელიწადი?(ით|ი|ში|ის|ს)?|წე?ლი?(ით|ის|ში|ს|იდან)?", Year)
         ]

rules :: [Rule]
rules = map go grains
  where
    go (name, regexPattern, grain) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = const $ Just $ Token TimeGrain grain
      }
