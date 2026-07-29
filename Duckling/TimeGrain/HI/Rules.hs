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
grains = [ ("second (grain)", "(सेकंडों|सेकंड|सेकण्डों|सेकण्ड)", TG.Second)
         , ("minute (grain)", "(मिनटों|मिनट)", TG.Minute)
         , ("hour (grain)", "(घंटों|घंटे|घंटा)", TG.Hour)
         , ("day (grain)", "(दिनों|दिन|दिवस)", TG.Day)
         , ("week (grain)", "(सप्ताहों|सप्ताह|हफ़्तों|हफ़्ते|हफ़्ता|हफ्तों|हफ्ते|हफ्ता)", TG.Week)
         , ("month (grain)", "(महीनों|महीने|महीना|माह|मास)", TG.Month)
         , ("quarter (grain)", "(चौथाइयों|चौथाई)", TG.Quarter)
         , ("year (grain)", "(सालों|साल|वर्षों|वर्ष|बरस)", TG.Year)
         ]

rules :: [Rule]
rules = map go grains
  where
    go (name, regexPattern, grain) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> Just $ Token TimeGrain grain
      }
