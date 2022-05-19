-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.TimeGrain.CA.Rules
  ( rules ) where

import Data.Text (Text)
import Prelude
import Data.String

import Duckling.Dimensions.Types
import qualified Duckling.TimeGrain.Types as TG
import Duckling.Types

grains :: [(Text, String, TG.Grain)]
grains = [ ("segon (grain)", "seg(on)?s?", TG.Second)
         , ("minuts (grain)", "min(ut)?s?", TG.Minute)
         , ("hora (grain)", "h(or(a|es))?", TG.Hour)
         , ("dia (grain)", "d(í|i)(a|es)", TG.Day)
         , ("setmana (grain)", "setman(a|es)", TG.Week)
         , ("mes (grain)", "mes(os)?", TG.Month)
         , ("trimestre (grain)", "trimestres?", TG.Quarter)
         , ("any (grain)", "anys?", TG.Year)
         ]

rules :: [Rule]
rules = map go grains
  where
    go (name, regexPattern, grain) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> Just $ Token TimeGrain grain
      }
