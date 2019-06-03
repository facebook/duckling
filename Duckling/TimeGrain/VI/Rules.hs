-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.

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
grains = [ ("second (grain)", "(giây|s|sec)", TG.Second)
         , ("minute (grain)", "(phút|m|min)", TG.Minute)
         , ("hour (grain)", "(giờ|h|tiếng)", TG.Hour)
         , ("day (grain)", "ngày", TG.Day)
         , ("week (grain)", "tuần", TG.Week)
         , ("month (grain)", "tháng", TG.Month)
         , ("quarter (grain)", "quý", TG.Quarter)
         , ("year (grain)", "năm", TG.Year)
         ]

rules :: [Rule]
rules = map go grains
  where
    go (name, regexPattern, grain) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> Just $ Token TimeGrain grain
      }
