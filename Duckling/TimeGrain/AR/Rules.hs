-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.TimeGrain.AR.Rules
  ( rules ) where

import Data.Text (Text)
import Prelude
import Data.String

import Duckling.Dimensions.Types
import qualified Duckling.TimeGrain.Types as TG
import Duckling.Types

grains :: [(Text, String, TG.Grain)]
grains = [ ("second (grain) ", "ثاني(ة|ه|تان|تين)|ثواني|لحظ(ة|ه|تان|تين|ات)",      TG.Second)
         , ("minute (grain)" , "دق(يق(ة|ه|تان|تين)|ائق)",      TG.Minute)
         , ("hour (grain)"   , "ساع(ة|ه|تين|تان|ات)", TG.Hour)
         , ("day (grain)"    , "يوم(|ان|ين)|(ا|أ)يام",            TG.Day)
         , ("week (grain)"   , "(ا|أ|إ)س(بوع(|ان|ين)|ابيع)",           TG.Week)
         , ("month (grain)"  , "شهر(|ان|ين)|(ا|أ|إ)شهر",          TG.Month)
         , ("year (grain)"   , "سن(ة|تين|تان|ين)",           TG.Year)
         ]

rules :: [Rule]
rules = map go grains
  where
    go (name, regexPattern, grain) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> Just $ Token TimeGrain grain
      }
