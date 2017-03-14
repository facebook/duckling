-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Rules.Common
  ( rules
  ) where

import Duckling.Dimensions.Types
import qualified Duckling.Distance.Rules as Distance
import qualified Duckling.Duration.Rules as Duration
import qualified Duckling.Email.Rules as Email
import qualified Duckling.Finance.Rules as Finance
import qualified Duckling.PhoneNumber.Rules as PhoneNumber
import qualified Duckling.Temperature.Rules as Temperature
import Duckling.Types
import qualified Duckling.Url.Rules as Url
import qualified Duckling.Volume.Rules as Volume

rules :: Some Dimension -> [Rule]
rules (Some Distance) = Distance.rules
rules (Some Duration) = Duration.rules
rules (Some Numeral) = []
rules (Some Email) = Email.rules
rules (Some Finance) = Finance.rules
rules (Some Ordinal) = []
rules (Some PhoneNumber) = PhoneNumber.rules
rules (Some Quantity) = []
rules (Some RegexMatch) = []
rules (Some Temperature) = Temperature.rules
rules (Some Time) = []
rules (Some TimeGrain) = []
rules (Some Url) = Url.rules
rules (Some Volume) = Volume.rules
