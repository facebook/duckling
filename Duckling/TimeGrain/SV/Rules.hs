-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.TimeGrain.SV.Rules
  ( rules ) where

import Data.Text (Text)
import Prelude
import Data.String

import Duckling.Dimensions.Types
import qualified Duckling.TimeGrain.Types as TG
import Duckling.Types

grains :: [(Text, String, TG.Grain)]
grains = [ ("second (grain)", "sek(und(er(na)?)?)?", TG.Second)
         , ("minute (grain)", "min(ut(er(na)?)?)?", TG.Minute)
         , ("hour (grain)", "t(imm(e(n)?|ar(na)?)?)?", TG.Hour)
         , ("day (grain)", "dag(en|ar(na)?)?", TG.Day)
         , ("week (grain)", "veck(or(na)?|a(n)?)?", TG.Week)
         , ("month (grain)", "månad(er(na)?)?", TG.Month)
         , ("quarter (grain)", "kvart(al)(et)?", TG.Quarter)
         , ("year (grain)", "år(e[nt])?", TG.Year)
         ]

rules :: [Rule]
rules = map go grains
  where
    go (name, regexPattern, grain) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> Just $ Token TimeGrain grain
      }
