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
import qualified Duckling.AmountOfMoney.Rules as AmountOfMoney
import qualified Duckling.PhoneNumber.Rules as PhoneNumber
import qualified Duckling.Temperature.Rules as Temperature
import Duckling.Types
import qualified Duckling.Url.Rules as Url
import qualified Duckling.Volume.Rules as Volume

rules :: Some Dimension -> [Rule]
rules (This Distance) = Distance.rules
rules (This Duration) = Duration.rules
rules (This Numeral) = []
rules (This Email) = Email.rules
rules (This AmountOfMoney) = AmountOfMoney.rules
rules (This Ordinal) = []
rules (This PhoneNumber) = PhoneNumber.rules
rules (This Quantity) = []
rules (This RegexMatch) = []
rules (This Temperature) = Temperature.rules
rules (This Time) = []
rules (This TimeGrain) = []
rules (This Url) = Url.rules
rules (This Volume) = Volume.rules
