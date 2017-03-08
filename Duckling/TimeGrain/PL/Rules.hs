-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


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
grains = [ ("second (grain)", "sekund(y|zie|(e|\x0119)|om|ami|ach|o|a)?|s", TG.Second)
         , ("minute (grain)", "minut(y|cie|(e|\x0119)|om|o|ami|ach|(a|\x0105))?|m", TG.Minute)
         , ("hour (grain)", "h|godzin(y|(e|\x0119)|ie|om|o|ami|ach|(a|\x0105))?", TG.Hour)
         , ("day (grain)", "dzie(n|\x0144|ni(a|\x0105))|dni(owi|ach|a|\x0105)?", TG.Day)
         , ("week (grain)", "tydzie(n|\x0144|)|tygod(ni(owi|u|a|em))|tygodn(iach|iami|iom|ie|i)|tyg\\.?", TG.Week)
         , ("month (grain)", "miesi(a|\x0105)c(owi|em|u|e|om|ami|ach|a)?", TG.Month)
         , ("quarter (grain)", "kwarta(l|\x0142)(u|owi|em|e|(o|\x00f3)w|om|ach|ami|y)?", TG.Quarter)
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
