-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.TimeGrain.PL.Rules
  ( rules ) where

import Data.Text (Text)
import Prelude
import Data.String

import Duckling.Dimensions.Types
import qualified Duckling.TimeGrain.Types as TG
import Duckling.Types

grains :: [(Text, String, TG.Grain)]
grains = [ ("second (grain)", "sekund(y|zie|(e|ę)|om|ami|ach|o|a)?|s", TG.Second)
         , ("minute (grain)", "minut(y|cie|(e|ę)|om|o|ami|ach|(a|ą))?|m", TG.Minute)
         , ("hour (grain)", "h|godzin(y|(e|ę)|ie|om|o|ami|ach|(a|ą))?", TG.Hour)
         , ("day (grain)", "dzie(n|ń|ni(a|ą))|dni(owi|ach|a|ą)?", TG.Day)
         , ("week (grain)", "tydzie(n|ń|)|tygod(ni(owi|u|a|em))|tygodn(iach|iami|iom|ie|i)|tyg\\.?", TG.Week)
         , ("month (grain)", "miesi(a|ą)c(owi|em|u|e|om|ami|ach|a)?", TG.Month)
         , ("quarter (grain)", "kwarta(l|ł)(u|owi|em|e|(o|ó)w|om|ach|ami|y)?", TG.Quarter)
         , ("year (grain)", "rok(u|owi|iem)?|lat(ami|ach|a|om)?", TG.Year)
         ]

rules :: [Rule]
rules = map go grains
  where
    go (name, regexPattern, grain) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> Just $ Token TimeGrain grain
      }
