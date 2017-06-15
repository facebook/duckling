-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.TimeGrain.ZH.Rules
  ( rules ) where

import Data.Text (Text)
import Prelude
import Data.String

import Duckling.Dimensions.Types
import qualified Duckling.TimeGrain.Types as TG
import Duckling.Types

grains :: [(Text, String, TG.Grain)]
grains = [ ("second (grain)", "\x79d2(\x949f|\x9418)?", TG.Second)
         , ("minute (grain)", "\x5206(\x949f|\x9418)?", TG.Minute)
         , ("hour (grain)",
             "\x5c0f\x65f6|\x5c0f\x6642|\x9418(\x982d)?", TG.Hour)
         , ("day (grain)", "\x5929|\x65e5", TG.Day)
         , ("week (grain)",
             "\x5468|\x9031|\x793c\x62dc|\x79ae\x62dc|\x661f\x671f", TG.Week)
         , ("month (grain)", "\x6708", TG.Month)
         , ("year (grain)", "\x5e74", TG.Year)
         ]

rules :: [Rule]
rules = map go grains
  where
    go (name, regexPattern, grain) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> Just $ Token TimeGrain grain
      }
