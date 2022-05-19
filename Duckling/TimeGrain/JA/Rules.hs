-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.TimeGrain.JA.Rules
  ( rules ) where

import Data.Text (Text)
import Prelude
import Data.String

import Duckling.Dimensions.Types
import qualified Duckling.TimeGrain.Types as TG
import Duckling.Types

grains :: [(Text, String, TG.Grain)]
grains = [ ("second (grain)", "秒(毎|間)?|びょう", TG.Second)
         , ("minute (grain)", "分(毎|間)?|ふん", TG.Minute)
         , ("hour (grain)", "時(毎|間)?|じ", TG.Hour)
         , ("day (grain)", "曜?日(毎|間)?|ようび|にち|ひ", TG.Day)
         , ("week (grain)", "週(毎|間)?|しゅう", TG.Week)
         , ("month (grain)", "月(毎|間)?|げつ|つき", TG.Month)
         , ("quarter (grain)", "(四|4)半期|しはんき|QTR", TG.Quarter)
         , ("year (grain)", "年(毎|間)?|ねん|とし", TG.Year)
         ]

rules :: [Rule]
rules = map go grains
  where
    go (name, regexPattern, grain) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> Just $ Token TimeGrain grain
      }
