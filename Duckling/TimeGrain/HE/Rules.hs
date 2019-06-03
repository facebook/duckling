-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.

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
grains = [ ("second (grain)", "שניות|שנייה", TG.Second)
         , ("minute (grain)", "דקה|דקות", TG.Minute)
         , ("hour (grain)", "שעות|שעה", TG.Hour)
         , ("day (grain)", "ימים|יום", TG.Day)
         , ("week (grain)", "שבוע|שבועות", TG.Week)
         , ("month (grain)", "חודש|חודשים", TG.Month)
         , ("quarter (grain)", "רבע", TG.Quarter)
         , ("year (grain)", "שנה", TG.Year)
         ]

rules :: [Rule]
rules = map go grains
  where
    go (name, regexPattern, grain) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> Just $ Token TimeGrain grain
      }
