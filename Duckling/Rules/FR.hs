-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Rules.FR
  ( rules
  ) where

import Duckling.Dimensions.Types
import qualified Duckling.Distance.FR.Rules as Distance
import qualified Duckling.Duration.FR.Rules as Duration
import qualified Duckling.Email.FR.Rules as Email
import qualified Duckling.Finance.FR.Rules as Finance
import qualified Duckling.Number.FR.Rules as Number
import qualified Duckling.Ordinal.FR.Rules as Ordinal
import qualified Duckling.Quantity.FR.Rules as Quantity
import qualified Duckling.Temperature.FR.Rules as Temperature
import qualified Duckling.Time.FR.Rules as Time
import qualified Duckling.TimeGrain.FR.Rules as TimeGrain
import qualified Duckling.Volume.FR.Rules as Volume
import Duckling.Types

rules :: Some Dimension -> [Rule]
rules (Some Distance) = Distance.rules
rules (Some Duration) = Duration.rules
rules (Some DNumber) = Number.rules
rules (Some Email) = Email.rules
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
