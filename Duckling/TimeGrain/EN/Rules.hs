-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.TimeGrain.EN.Rules
  ( rules
  ) where

import Data.String
import Data.Text (Text)
import Prelude

import Duckling.Dimensions.Types
import Duckling.Types
import qualified Duckling.TimeGrain.Types as TG

grains :: [(Text, String, TG.Grain)]
grains = [ ("second (grain) ", "sec(ond)?s?",      TG.Second)
         , ("minute (grain)" , "min(ute)?s?",      TG.Minute)
         , ("hour (grain)"   , "h(((ou)?rs?)|r)?", TG.Hour)
         , ("day (grain)"    , "days?",            TG.Day)
         , ("week (grain)"   , "weeks?",           TG.Week)
         , ("month (grain)"  , "months?",          TG.Month)
         , ("quarter (grain)", "(quarter|qtr)s?",  TG.Quarter)
         , ("year (grain)"   , "y(ea)?rs?",        TG.Year)
         ]

rules :: [Rule]
rules = map go grains
  where
    go (name, regexPattern, grain) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> Just $ Token TimeGrain grain
      }
