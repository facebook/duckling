-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.TimeGrain.TR.Rules
  ( rules
  ) where

import Data.String
import Data.Text (Text)
import Prelude

import Duckling.Dimensions.Types
import Duckling.Types
import qualified Duckling.TimeGrain.Types  as TG

grains :: [(Text, String, TG.Grain)]
grains = [ ("saniye (grain)",     "sa?n(iye)?",           TG.Second)
         , ("dakika (grain)",     "da?k(ika)?",           TG.Minute)
         , ("saat (grain)",       "sa(at)?",              TG.Hour)
         , ("gün (grain)",        "gün",             TG.Day)
         , ("hafta (grain)",      "hafta",                TG.Week)
         , ("ay (grain)",         "ay",                   TG.Month)
         , ("çeyrek yıl (grain)", "\231eyrek y\305l",     TG.Quarter)
         , ("yıl (grain)",        "y\305l",               TG.Year)
         ]

rules :: [Rule]
rules = map go grains
  where
    go (name, regexPattern, grain) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> Just $ Token TimeGrain grain
      }
