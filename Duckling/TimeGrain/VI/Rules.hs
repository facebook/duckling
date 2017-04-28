-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.TimeGrain.VI.Rules
  ( rules ) where

import Data.String
import Data.Text (Text)
import Prelude

import Duckling.Dimensions.Types
import Duckling.Types
import qualified Duckling.TimeGrain.Types as TG

grains :: [(Text, String, TG.Grain)]
grains = [ ("second (grain)", "(gi\x00e2y|s|sec)", TG.Second)
         , ("minute (grain)", "(ph\x00fat|m|min)", TG.Minute)
         , ("hour (grain)", "(gi\x1edd|h|ti\x1ebfng)", TG.Hour)
         , ("day (grain)", "ng\x00e0y", TG.Day)
         , ("week (grain)", "tu\x1ea7n", TG.Week)
         , ("month (grain)", "th\x00e1ng", TG.Month)
         , ("quarter (grain)", "qu\x00fd", TG.Quarter)
         , ("year (grain)", "n\x0103m", TG.Year)
         ]

rules :: [Rule]
rules = map go grains
  where
    go (name, regexPattern, grain) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> Just $ Token TimeGrain grain
      }
