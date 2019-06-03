-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.TimeGrain.HR.Rules
  ( rules ) where

import Data.String
import Data.Text (Text)
import Prelude

import Duckling.Dimensions.Types
import Duckling.Types
import qualified Duckling.TimeGrain.Types as TG

grains :: [(Text, String, TG.Grain)]
grains = [ ("second (grain)", "sek(und)?(a|e|u)?", TG.Second)
         , ("minute (grain)", "min(ut)?(a|e|u)?", TG.Minute)
         , ("hour (grain)", "h|sat(i|a|e)?", TG.Hour)
         , ("day (grain)", "dan(i|a|e)?", TG.Day)
         , ("week (grain)", "tjeda?n(a|e|u|i)?", TG.Week)
         , ("month (grain)", "mjesec(a|e|u|i)?", TG.Month)
         , ("quarter (grain)", "kvartalu?|tromjese(c|č)j(e|u)", TG.Quarter)
         , ("year (grain)", "godin(a|e|u)", TG.Year)
         ]

rules :: [Rule]
rules = map go grains
  where
    go (name, regexPattern, grain) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> Just $ Token TimeGrain grain
      }
