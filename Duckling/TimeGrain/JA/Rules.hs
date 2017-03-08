-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


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
grains = [ ("second (grain)", "\x79d2(\x6bce|\x9593)?", TG.Second)
         , ("minute (grain)", "\x5206(\x6bce|\x9593)?", TG.Minute)
         , ("hour (grain)", "\x6642(\x6bce|\x9593)?", TG.Hour)
         , ("day (grain)", "\x65e5(\x6bce|\x9593)?", TG.Day)
         , ("week (grain)", "\x9031(\x6bce|\x9593)?", TG.Week)
         , ("month (grain)", "\x6708(\x6bce|\x9593)?", TG.Month)
         , ("year (grain)", "\x5e74(\x6bce|\x9593)?", TG.Year)
         ]

rules :: [Rule]
rules = map go grains
  where
    go (name, regexPattern, grain) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> Just $ Token TimeGrain grain
      }
