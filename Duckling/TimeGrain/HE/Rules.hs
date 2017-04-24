-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.TimeGrain.HE.Rules
  ( rules ) where

import Data.String
import Data.Text (Text)
import Prelude

import Duckling.Dimensions.Types
import Duckling.Types
import qualified Duckling.TimeGrain.Types as TG

grains :: [(Text, String, TG.Grain)]
grains = [ ("second (grain)", "\x05e9\x05e0\x05d9\x05d5\x05ea|\x05e9\x05e0\x05d9\x05d9\x05d4", TG.Second)
         , ("minute (grain)", "\x05d3\x05e7\x05d4|\x05d3\x05e7\x05d5\x05ea", TG.Minute)
         , ("hour (grain)", "\x05e9\x05e2\x05d5\x05ea|\x05e9\x05e2\x05d4", TG.Hour)
         , ("day (grain)", "\x05d9\x05de\x05d9\x05dd|\x05d9\x05d5\x05dd", TG.Day)
         , ("week (grain)", "\x05e9\x05d1\x05d5\x05e2|\x05e9\x05d1\x05d5\x05e2\x05d5\x05ea", TG.Week)
         , ("month (grain)", "\x05d7\x05d5\x05d3\x05e9|\x05d7\x05d5\x05d3\x05e9\x05d9\x05dd", TG.Month)
         , ("quarter (grain)", "\x05e8\x05d1\x05e2", TG.Quarter)
         , ("year (grain)", "\x05e9\x05e0\x05d4", TG.Year)
         ]

rules :: [Rule]
rules = map go grains
  where
    go (name, regexPattern, grain) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> Just $ Token TimeGrain grain
      }
