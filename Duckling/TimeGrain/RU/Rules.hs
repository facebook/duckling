-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.TimeGrain.RU.Rules
  ( rules
  ) where

import Data.String
import Data.Text (Text)
import Prelude

import Duckling.Dimensions.Types
import Duckling.TimeGrain.Types
import Duckling.Types

grains :: [(Text, String, Grain)]
grains = [ ("second (grain) ", "сек(унд(а|у|ы)?)?",   Second)
         , ("minute (grain)" , "мин(ут(а|у|ы)?)?",    Minute)
         , ("hour (grain)"   , "ч(ас(а|ов)?)?",       Hour)
         , ("day (grain)"    , "день|дня|дней",       Day)
         , ("week (grain)"   , "недел(ь|я|и|ю)?",     Week)
         , ("month (grain)"  , "месяц(а|ев)?",        Month)
         , ("quarter (grain)", "квартал(а)?",         Quarter)
         , ("year (grain)"   , "года?|лет",           Year)
         ]

rules :: [Rule]
rules = map go grains
  where
    go (name, regexPattern, grain) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> Just $ Token TimeGrain grain
      }
