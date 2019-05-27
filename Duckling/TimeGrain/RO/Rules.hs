-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.TimeGrain.RO.Rules
  ( rules
  ) where

import Data.String
import Data.Text (Text)
import Prelude

import Duckling.Dimensions.Types
import Duckling.Types
import qualified Duckling.TimeGrain.Types as TG

grains :: [(Text, String, TG.Grain)]
grains = [ ("secunde (grain)", "sec(und(a|e|ă))?", TG.Second)
         , ("minute (grain)", "min(ut(e|ul)?)?", TG.Minute)
         , ("ore (grain)", "h|or(a|e(le)?|ă)", TG.Hour)
         , ("zile (grain)", "zi(le(le)?|u(a|ă))?", TG.Day)
         , ("saptamani (grain)", "sapt(a|ă)m(a|â)n(ile|a|ă|i)", TG.Week)
         , ("luni (grain)", "lun(i(le)?|a|ă)", TG.Month)
         , ("trimestru (grain)", "trimestr(e(le)?|ul?)", TG.Quarter)
         , ("ani (grain)", "an(ul|ii?)?", TG.Year)
         ]

rules :: [Rule]
rules = map go grains
  where
    go (name, regexPattern, grain) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> Just $ Token TimeGrain grain
      }
