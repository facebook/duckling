-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Rules.RO
  ( rules
  ) where

import Duckling.Dimensions.Types
import qualified Duckling.Distance.RO.Rules as Distance
import qualified Duckling.Duration.RO.Rules as Duration
import qualified Duckling.Finance.RO.Rules as Finance
import qualified Duckling.Number.RO.Rules as Number
import qualified Duckling.Ordinal.RO.Rules as Ordinal
import qualified Duckling.Quantity.RO.Rules as Quantity
import qualified Duckling.Temperature.RO.Rules as Temperature
import qualified Duckling.Time.RO.Rules as Time
import qualified Duckling.TimeGrain.RO.Rules as TimeGrain
import qualified Duckling.Volume.RO.Rules as Volume
import Duckling.Types

rules :: Some Dimension -> [Rule]
rules (Some Distance) = Distance.rules
rules (Some Duration) = Duration.rules
rules (Some DNumber) = Number.rules
rules (Some Email) = []
rules (Some Finance) = Finance.rules
rules (Some Ordinal) = Ordinal.rules
rules (Some PhoneNumber) = []
rules (Some Quantity) = Quantity.rules
rules (Some RegexMatch) = []
rules (Some Temperature) = Temperature.rules
rules (Some Time) = Time.rules
rules (Some TimeGrain) = TimeGrain.rules
rules (Some Url) = []
rules (Some Volume) = Volume.rules
