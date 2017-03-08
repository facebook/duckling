-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.TimeGrain.RO.Rules
  ( rules ) where

import Data.Text (Text)
import Prelude
import Data.String

import Duckling.Dimensions.Types
import qualified Duckling.TimeGrain.Types as TG
import Duckling.Types

grains :: [(Text, String, TG.Grain)]
grains = [ ("secunde (grain)", "sec(und(a|e|\x0103))?", TG.Second)
         , ("minute (grain)", "min(ut(e|ul)?)?", TG.Minute)
         , ("ore (grain)", "h|or(a|e(le)?|\x0103)", TG.Hour)
         , ("zile (grain)", "zi(le(le)?|u(a|\x0103))?", TG.Day)
         , ("saptamani (grain)", "sapt(a|\x0103)m(a|\x00e2)n(ile|a|\x0103|i)", TG.Week)
         , ("luni (grain)", "lun(i(le)?|a|\x0103)", TG.Month)
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
