-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Rules.PT
  ( defaultRules
  , langRules
  , localeRules
  ) where

import Duckling.Dimensions.Types
import Duckling.Locale
import Duckling.Types
import qualified Duckling.AmountOfMoney.PT.Rules as AmountOfMoney
import qualified Duckling.Distance.PT.Rules as Distance
import qualified Duckling.Numeral.PT.Rules as Numeral
import qualified Duckling.Ordinal.PT.Rules as Ordinal
import qualified Duckling.PhoneNumber.PT.Rules as PhoneNumber
import qualified Duckling.Quantity.PT.Rules as Quantity
import qualified Duckling.Temperature.PT.Rules as Temperature
import qualified Duckling.Time.PT.Rules as Time
import qualified Duckling.TimeGrain.PT.Rules as TimeGrain
import qualified Duckling.Volume.PT.Rules as Volume

defaultRules :: Some Dimension -> [Rule]
defaultRules = langRules

localeRules :: Region -> Some Dimension -> [Rule]
localeRules _ _ = []

langRules :: Some Dimension -> [Rule]
langRules (This AmountOfMoney) = AmountOfMoney.rules
langRules (This Distance) = Distance.rules
langRules (This Duration) = []
langRules (This Email) = []
langRules (This Numeral) = Numeral.rules
langRules (This Ordinal) = Ordinal.rules
langRules (This PhoneNumber) = PhoneNumber.rules
langRules (This Quantity) = Quantity.rules
langRules (This RegexMatch) = []
langRules (This Temperature) = Temperature.rules
langRules (This Time) = Time.rules
langRules (This TimeGrain) = TimeGrain.rules
langRules (This Url) = []
langRules (This Volume) = Volume.rules
