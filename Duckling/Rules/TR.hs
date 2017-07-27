-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Rules.TR
  ( rules
  ) where

import Duckling.Dimensions.Types
import Duckling.Types
import qualified Duckling.Distance.TR.Rules as Distance
import qualified Duckling.Duration.TR.Rules as Duration
import qualified Duckling.Numeral.TR.Rules as Numeral
import qualified Duckling.Ordinal.TR.Rules as Ordinal
import qualified Duckling.Temperature.TR.Rules as Temperature
import qualified Duckling.TimeGrain.TR.Rules as TimeGrain
import qualified Duckling.Volume.TR.Rules as Volume

rules :: Some Dimension -> [Rule]
rules (This Distance) = Distance.rules
rules (This Duration) = Duration.rules
rules (This Numeral) = Numeral.rules
rules (This Email) = []
rules (This AmountOfMoney) = []
rules (This Ordinal) = Ordinal.rules
rules (This PhoneNumber) = []
rules (This Quantity) = []
rules (This RegexMatch) = []
rules (This Temperature) = Temperature.rules
rules (This Time) = []
rules (This TimeGrain) = TimeGrain.rules
rules (This Url) = []
rules (This Volume) = Volume.rules
