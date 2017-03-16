-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Rules.EN
  ( rules
  ) where

import Duckling.Dimensions.Types
import qualified Duckling.AmountOfMoney.EN.Rules as AmountOfMoney
import qualified Duckling.Distance.EN.Rules as Distance
import qualified Duckling.Duration.EN.Rules as Duration
import qualified Duckling.Email.EN.Rules as Email
import qualified Duckling.Numeral.EN.Rules as Numeral
import qualified Duckling.Ordinal.EN.Rules as Ordinal
import qualified Duckling.Quantity.EN.Rules as Quantity
import qualified Duckling.Temperature.EN.Rules as Temperature
import qualified Duckling.Time.EN.Rules as Time
import qualified Duckling.TimeGrain.EN.Rules as TimeGrain
import Duckling.Types
import qualified Duckling.Volume.EN.Rules as Volume

rules :: Some Dimension -> [Rule]
rules (This Distance) = Distance.rules
rules (This Duration) = Duration.rules
rules (This Numeral) = Numeral.rules
rules (This Email) = Email.rules
rules (This AmountOfMoney) = AmountOfMoney.rules
rules (This Ordinal) = Ordinal.rules
rules (This PhoneNumber) = []
rules (This Quantity) = Quantity.rules
rules (This RegexMatch) = []
rules (This Temperature) = Temperature.rules
rules (This Time) = Time.rules
rules (This TimeGrain) = TimeGrain.rules
rules (This Url) = []
rules (This Volume) = Volume.rules
