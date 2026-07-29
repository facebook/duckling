-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.TimeGrain.MR.Rules
  ( rules
  ) where

import Data.String
import Data.Text (Text)
import Prelude

import Duckling.Dimensions.Types
import Duckling.Types
import qualified Duckling.TimeGrain.Types as TG

grains :: [(Text, String, TG.Grain)]
grains = [ ("second (grain)", "(सेकंदांना|सेकंदांत|सेकंदां|सेकंदात|सेकंदाने|सेकंद)", TG.Second)
         , ("minute (grain)", "(मिनिटांना|मिनिटांत|मिनिटांची|मिनिटांचा|मिनिटांचे|मिनिटां|मिनिटे|मिनिट)", TG.Minute)
         , ("hour (grain)", "(तासांना|तासांत|तासाचे|तासाची|तासाचा|तासां|तासाने|तास)", TG.Hour)
         , ("day (grain)", "(दिवसांना|दिवसांत|दिवसाचे|दिवसाची|दिवसाचा|दिवसां|दिवशी|दिवस)", TG.Day)
         , ("week (grain)", "(आठवड्यांच्या|आठवड्याचा|आठवड्याची|आठवड्याचे|आठवड्यातील|आठवड्यात|आठवड्यांत|आठवड्या|आठवडे|आठवडा|सप्ताहात|सप्ताह)", TG.Week)
         , ("month (grain)", "(महिन्यांच्या|महिन्याच्या|महिन्यातील|महिन्यात|महिन्यांत|महिन्या|महिने|महिना|मास)", TG.Month)
         , ("quarter (grain)", "तिमाही", TG.Quarter)
         , ("year (grain)", "(वर्षांच्या|वर्षाच्या|वर्षातील|वर्षात|वर्षांत|वर्षां|वर्ष|सालात|साल)", TG.Year)
         ]

rules :: [Rule]
rules = map go grains
  where
    go (name, regexPattern, grain) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> Just $ Token TimeGrain grain
      }
