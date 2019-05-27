-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.TimeGrain.UK.Rules
  ( rules ) where

import Data.String
import Data.Text (Text)
import Prelude

import Duckling.Dimensions.Types
import Duckling.Types
import qualified Duckling.TimeGrain.Types as TG

grains :: [(Text, String, TG.Grain)]
grains = [ ("second (grain)", "секунд(а|и|і|у)?|сек", TG.Second)
         , ("minute (grain)", "хвилин(а|и|і|у)?|хв", TG.Minute)
         , ("hour (grain)", "годин(а|и|і|у)?", TG.Hour)
         , ("day (grain)", "день|дн(і|я)в?", TG.Day)
         , ("week (grain)", "тиждень|тижн(я|і)в?", TG.Week)
         , ("month (grain)", "місяц(ь|я|і)в?", TG.Month)
         , ("quarter (grain)", "квартал(и|і)?в?", TG.Quarter)
         , ("year (grain)", "рік|ро(к|ц)(и|і|у)?в?", TG.Year)
         ]

rules :: [Rule]
rules = map go grains
  where
    go (name, regexPattern, grain) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> Just $ Token TimeGrain grain
      }
