-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.TimeGrain.HI.Rules
  ( rules
  ) where

import Data.String
import Data.Text (Text)
import Prelude

import Duckling.Dimensions.Types
import Duckling.Types
import qualified Duckling.TimeGrain.Types as TG

grains :: [(Text, String, TG.Grain)]
grains = [ ("second (grain)", "सेकंड", TG.Second)
         , ("minute (grain)", "मिनट", TG.Minute)
         , ("hour (grain)", "घंटा", TG.Hour)
         , ("day (grain)", "(दिन|दिवस)", TG.Day)
         , ("week (grain)", "(सप्ताह|हफ़्ता)", TG.Week)
         , ("month (grain)", "महीना", TG.Month)
         , ("quarter (grain)", "चौथाई", TG.Quarter)
         , ("year (grain)", "(साल|वर्ष)", TG.Year)
         ]

rules :: [Rule]
rules = map go grains
  where
    go (name, regexPattern, grain) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> Just $ Token TimeGrain grain
      }
