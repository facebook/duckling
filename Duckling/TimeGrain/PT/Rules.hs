-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.TimeGrain.PT.Rules
  ( rules
  ) where

import Data.Text (Text)
import Data.String
import Prelude

import Duckling.Dimensions.Types
import Duckling.Types
import qualified Duckling.TimeGrain.Types as TG

grains :: [(Text, String, TG.Grain)]
grains = [ ("segundo (grain)", "seg(undo)?s?", TG.Second)
         , ("minutos (grain)", "min(uto)?s?", TG.Minute)
         , ("hora (grain)", "h(ora)?s?", TG.Hour)
         , ("dia (grain)", "d(í|i)as?", TG.Day)
         , ("semana (grain)", "semanas?", TG.Week)
         , ("mes (grain)", "m(e|ê)s(es)?", TG.Month)
         , ("trimestre (grain)", "trimestres?", TG.Quarter)
         , ("ano (grain)", "anos?", TG.Year)
         ]

rules :: [Rule]
rules = map go grains
  where
    go (name, regexPattern, grain) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> Just $ Token TimeGrain grain
      }
