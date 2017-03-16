-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Rules.NB
  ( rules
  ) where

import Duckling.Dimensions.Types
import qualified Duckling.AmountOfMoney.NB.Rules as AmountOfMoney
import qualified Duckling.Duration.NB.Rules as Duration
import qualified Duckling.Numeral.NB.Rules as Numeral
import qualified Duckling.Ordinal.NB.Rules as Ordinal
import qualified Duckling.Time.NB.Rules as Time
import qualified Duckling.TimeGrain.NB.Rules as TimeGrain
import Duckling.Types

rules :: Some Dimension -> [Rule]
rules (This Distance) = []
rules (This Duration) = Duration.rules
rules (This Numeral) = Numeral.rules
rules (This Email) = []
rules (This AmountOfMoney) = AmountOfMoney.rules
rules (This Ordinal) = Ordinal.rules
rules (This PhoneNumber) = []
rules (This Quantity) = []
rules (This RegexMatch) = []
rules (This Temperature) = []
rules (This Time) = Time.rules
rules (This TimeGrain) = TimeGrain.rules
rules (This Url) = []
rules (This Volume) = []
