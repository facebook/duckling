-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


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
grains = [ ("second (grain)", "\xcd08", TG.Second)
         , ("minute (grain)", "\xbd84", TG.Minute)
         , ("hour (grain)", "\xc2dc(\xac04)?", TG.Hour)
         , ("day (grain)", "\xb0a0|\xc77c(\xac04|\xb3d9\xc548)?", TG.Day)
         , ("week (grain)", "\xc8fc(\xac04|\xb3d9\xc548|\xc77c)?", TG.Week)
         , ("month (grain)", "(\xb2ec)(\xac04|\xb3d9\xc548)?", TG.Month)
         , ("quarter (grain)", "\xbd84\xae30(\xac04|\xb3d9\xc548)?", TG.Quarter)
         , ("year (grain)", "\xd574|\xc5f0\xac04|\xb144(\xac04|\xb3d9\xc548)?", TG.Year)
         ]

rules :: [Rule]
rules = map go grains
  where
    go (name, regexPattern, grain) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> Just $ Token TimeGrain grain
      }
