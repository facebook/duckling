-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.TimeGrain.FI.Rules
  ( rules
  ) where

import Data.String
import Data.Text (Text)
import Prelude

import Duckling.Dimensions.Types
import Duckling.Types
import qualified Duckling.TimeGrain.Types as TG

grains :: [(Text, String, TG.Grain)]
grains = [ ("second (grain) ", "sekuntia?|s", TG.Second)
         , ("minute (grain)" , "minuuttia?|min", TG.Minute)
         , ("hour (grain)"   , "tuntia?|h", TG.Hour)
         , ("day (grain)"    , "päivä(ä)?|vuorokau(si|tta)|vrk|d", TG.Day)
         , ("week (grain)"   , "viikkoa?|vko?", TG.Week)
         , ("month (grain)"  , "kuukau(si|tta)|kk", TG.Month)
         , ("quarter (grain)", "kvartaali(a)?|neljännesvuo(si|tta)", TG.Quarter)
         , ("year (grain)"   , "vuo(si|tta)|v\\.?|a", TG.Year)
         ]

rules :: [Rule]
rules = map go grains
  where
    go (name, regexPattern, grain) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> Just $ Token TimeGrain grain
      }
