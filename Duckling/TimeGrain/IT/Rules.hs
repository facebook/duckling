-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.TimeGrain.IT.Rules
  ( rules ) where

import Data.Text (Text)
import Prelude
import Data.String

import Duckling.Dimensions.Types
import qualified Duckling.TimeGrain.Types as TG
import Duckling.Types

grains :: [(Text, String, TG.Grain)]
grains = [ ("seconde (grain)", "second[oi]", TG.Second)
         , ("minute (grain)", "minut[oi]", TG.Minute)
         , ("heure (grain)", "or[ae]", TG.Hour)
         , ("jour (grain)", "giorn[oi]", TG.Day)
         , ("semaine (grain)", "settiman[ae]", TG.Week)
         , ("mois (grain)", "mes[ei]", TG.Month)
         , ("trimestre (grain)", "trimestr[ei]", TG.Quarter)
         , ("année (grain)", "ann?[oi]", TG.Year)
         ]

rules :: [Rule]
rules = map go grains
  where
    go (name, regexPattern, grain) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> Just $ Token TimeGrain grain
      }
