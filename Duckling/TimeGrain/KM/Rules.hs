-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.TimeGrain.KM.Rules
  ( rules
  ) where

import Data.String
import Data.Text (Text)
import Prelude

import Duckling.Dimensions.Types
import Duckling.Types
import qualified Duckling.TimeGrain.Types as TG

grains :: [(Text, String, TG.Grain)]
grains = [ ("second (grain) ", "វិ(នាទី)?",      TG.Second)
         , ("minute (grain)" , "នាទី",      TG.Minute)
         , ("hour (grain)"   , "ម៉ោង", TG.Hour)
         , ("day (grain)"    , "ថ្ងៃ",            TG.Day)
         , ("week (grain)"   , "សប្ដាហ៍|អាទិត្យ",           TG.Week)
         , ("month (grain)"  , "ខែ",          TG.Month)
         , ("quarter (grain)", "ត្រីមាស",  TG.Quarter)
         , ("year (grain)"   , "ឆ្នាំ",        TG.Year)
         ]

rules :: [Rule]
rules = map go grains
  where
    go (name, regexPattern, grain) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> Just $ Token TimeGrain grain
      }
