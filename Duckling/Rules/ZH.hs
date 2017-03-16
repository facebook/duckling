-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Rules.ZH
  ( rules
  ) where

import Duckling.Dimensions.Types
import qualified Duckling.Numeral.ZH.Rules as Numeral
import qualified Duckling.Ordinal.ZH.Rules as Ordinal
import qualified Duckling.Temperature.ZH.Rules as Temperature
import qualified Duckling.Time.ZH.Rules as Time
import qualified Duckling.TimeGrain.ZH.Rules as TimeGrain
import Duckling.Types

rules :: Some Dimension -> [Rule]
rules (This Distance) = []
rules (This Duration) = []
rules (This Numeral) = Numeral.rules
rules (This Email) = []
rules (This AmountOfMoney) = []
rules (This Ordinal) = Ordinal.rules
rules (This PhoneNumber) = []
rules (This Quantity) = []
rules (This RegexMatch) = []
rules (This Temperature) = Temperature.rules
rules (This Time) = Time.rules
rules (This TimeGrain) = TimeGrain.rules
rules (This Url) = []
rules (This Volume) = []
