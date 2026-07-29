-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.TimeGrain.GU.Rules
  ( rules
  ) where

import Data.String
import Data.Text (Text)
import Prelude

import Duckling.Dimensions.Types
import Duckling.Types
import qualified Duckling.TimeGrain.Types as TG

grains :: [(Text, String, TG.Grain)]
grains = [ ("second (grain)", "(સેકન્ડો|સેકન્ડ|સેકંડ)", TG.Second)
         , ("minute (grain)", "(મિનિટો|મિનિટની|મિનિટ|ક્ષણ)", TG.Minute)
         , ("hour (grain)", "(કલાકો|કલાકે|કલાક)", TG.Hour)
         , ("day (grain)", "(દિવસો|દિવસે|દિવસ|દિન|દિને)", TG.Day)
         , ("week (grain)", "(અઠવાડિયાં|અઠવાડિયા|અઠવાડિયું|અઠવાડિયે|સપ્તાહ|સપ્તાહે)", TG.Week)
         , ("month (grain)", "(મહિનાઓ|મહિનાના|મહિના|મહિનો|મહિને|માસ|માસે)", TG.Month)
         , ("quarter (grain)", "ત્રિમાસ", TG.Quarter)
         , ("year (grain)", "(વર્ષો|વર્ષે|વર્ષ|સાલે|સાલ)", TG.Year)
         ]

rules :: [Rule]
rules = map go grains
  where
    go (name, regexPattern, grain) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> Just $ Token TimeGrain grain
      }
