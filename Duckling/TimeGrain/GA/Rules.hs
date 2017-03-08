-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


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
grains = [ ("soicind (grain)", "t?sh?oicind(\x00ed|i)?", TG.Second)
         , ("nóiméad (grain)", "n[\x00f3o]im(\x00e9|e)[ai]da?", TG.Minute)
         , ("uair (grain)", "([thn]-?)?uair(e|eanta)?", TG.Hour)
         , ("lá (grain)", "l(ae(thanta)?|(\x00e1|a))", TG.Day)
         , ("seachtain (grain)", "t?sh?eachtain(e|(\x00ed|i))?", TG.Week)
         , ("mí (grain)", "mh?(\x00ed|i)(sa|nna)", TG.Month)
         , ("ráithe (grain)", "r(\x00e1|a)ith(e|(\x00ed|i))", TG.Quarter)
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
