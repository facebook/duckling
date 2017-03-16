-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Rules.PL
  ( rules
  ) where

import Duckling.Dimensions.Types
import qualified Duckling.Duration.PL.Rules as Duration
import qualified Duckling.Numeral.PL.Rules as Numeral
import qualified Duckling.Ordinal.PL.Rules as Ordinal
import qualified Duckling.Time.PL.Rules as Time
import qualified Duckling.TimeGrain.PL.Rules as TimeGrain
import Duckling.Types

rules :: Some Dimension -> [Rule]
rules (This Distance) = []
rules (This Duration) = Duration.rules
rules (This Numeral) = Numeral.rules
rules (This Email) = []
rules (This AmountOfMoney) = []
rules (This Ordinal) = Ordinal.rules
rules (This PhoneNumber) = []
rules (This Quantity) = []
rules (This RegexMatch) = []
rules (This Temperature) = []
rules (This Time) = Time.rules
rules (This TimeGrain) = TimeGrain.rules
rules (This Url) = []
rules (This Volume) = []
