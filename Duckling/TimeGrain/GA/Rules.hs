-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.TimeGrain.GA.Rules
  ( rules ) where

import Data.Text (Text)
import Prelude
import Data.String

import Duckling.Dimensions.Types
import qualified Duckling.TimeGrain.Types as TG
import Duckling.Types

grains :: [(Text, String, TG.Grain)]
grains = [ ("soicind (grain)", "t?sh?oicind(í|i)?", TG.Second)
         , ("nóiméad (grain)", "n[óo]im(é|e)[ai]da?", TG.Minute)
         , ("uair (grain)", "([thn]-?)?uair(e|eanta)?", TG.Hour)
         , ("lá (grain)", "l(ae(thanta)?|(á|a))", TG.Day)
         , ("seachtain (grain)", "t?sh?eachtain(e|(í|i))?", TG.Week)
         , ("mí (grain)", "mh?(í|i)(sa|nna)", TG.Month)
         , ("ráithe (grain)", "r(á|a)ith(e|(í|i))", TG.Quarter)
         , ("bliain (grain)", "m?bh?lia(in|na|nta)", TG.Year)
         ]

rules :: [Rule]
rules = map go grains
  where
    go (name, regexPattern, grain) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> Just $ Token TimeGrain grain
      }
