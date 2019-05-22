-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.TimeGrain.NL.Rules
  ( rules ) where

import Data.Text (Text)
import Data.String
import Prelude

import Duckling.Dimensions.Types
import Duckling.Types
import qualified Duckling.TimeGrain.Types as TG

grains :: [(Text, String, TG.Grain)]
grains = [ ("second (grain) " , "(seconde(n|s)?|sec|s)" , TG.Second)
         , ("minute (grain)"  , "(minuut|minuten|min|m)", TG.Minute)
         , ("hour (grain)"    , "(uur|uren|u|h)",         TG.Hour)
         , ("day (grain)"     , "dagen|dag|d",            TG.Day)
         , ("week (grain)"    , "(weken|week|w)",         TG.Week)
         , ("month (grain)"   , "(maanden|maand|mnd)",    TG.Month)
         , ("quarter (grain)" , "(kwartaal|kwartalen)",   TG.Quarter)
         , ("year (grain)"    , "(jaren|jaar|j)",         TG.Year)
         ]

rules :: [Rule]
rules = map go grains
  where
    go (name, regexPattern, grain) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> Just $ Token TimeGrain grain
      }
