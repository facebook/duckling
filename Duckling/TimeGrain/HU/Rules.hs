-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.TimeGrain.HU.Rules
  ( rules ) where

import Data.String
import Data.Text (Text)
import Prelude

import Duckling.Dimensions.Types
import Duckling.Types
import qualified Duckling.TimeGrain.Types as TG

grains :: [(Text, String, TG.Grain)]
grains = [ ("second (grain) ", "m\x00E1sodperc(ek)?|mp",    TG.Second)
         , ("minute (grain)" , "perc(ek)?",                 TG.Minute)
         , ("hour (grain)"   , "\x00F3ra|\x00F3r\x00E1k",   TG.Hour)
         , ("day (grain)"    , "nap(ok)?",                  TG.Day)
         , ("week (grain)"   , "h\x00E9t|hetek",            TG.Week)
         , ("month (grain)"  , "h\x00F3nap|h\x00F3napok",   TG.Month)
         , ("quarter (grain)", "negyed\\s?\x00E9v(ek)?",    TG.Quarter)
         , ("year (grain)"   , "\x00E9v(ek)?",              TG.Year)
         ]

rules :: [Rule]
rules = map go grains
  where
    go (name, regexPattern, grain) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> Just $ Token TimeGrain grain
      }
