-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.TimeGrain.MN.Rules
  ( rules
  ) where

import Data.String
import Data.Text (Text)
import Prelude

import Duckling.Dimensions.Types
import Duckling.TimeGrain.Types
import Duckling.Types

grains :: [(Text, String, Grain)]
grains = [ ("second (grain) ", "сек(унд)?",   Second)
         , ("minute (grain)" , "мин(ут)?",    Minute)
         , ("hour (grain)"   , "ц(аг)?",       Hour)
         , ("day (grain)"    , "өдөр?",       Day)
         , ("week (grain)"   , "долоо хоног?",     Week)
         , ("month (grain)"  , "сар?",        Month)
         , ("quarter (grain)", "улирал?",         Quarter)
         , ("year (grain)"   , "жил?|жил",           Year)
         ]

rules :: [Rule]
rules = map go grains
  where
    go (name, regexPattern, grain) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> Just $ Token TimeGrain grain
      }
