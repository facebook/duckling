-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}


module Duckling.Rules.ZH
  ( defaultRules
  , langRules
  , localeRules
  ) where

import Duckling.Dimensions.Types
import Duckling.Locale
import Duckling.Types
import qualified Duckling.Distance.ZH.Rules as Distance
import qualified Duckling.Numeral.ZH.Rules as Numeral
import qualified Duckling.Ordinal.ZH.Rules as Ordinal
import qualified Duckling.Temperature.ZH.Rules as Temperature
import qualified Duckling.Time.ZH.Rules as Time
import qualified Duckling.Time.ZH.CN.Rules as TimeCN
import qualified Duckling.Time.ZH.HK.Rules as TimeHK
import qualified Duckling.Time.ZH.MO.Rules as TimeMO
import qualified Duckling.Time.ZH.TW.Rules as TimeTW
import qualified Duckling.TimeGrain.ZH.Rules as TimeGrain

defaultRules :: Some Dimension -> [Rule]
defaultRules = langRules

localeRules :: Region -> Some Dimension -> [Rule]
localeRules CN (This Time) = TimeCN.rules
localeRules HK (This Time) = TimeHK.rules
localeRules MO (This Time) = TimeMO.rules
localeRules TW (This Time) = TimeTW.rules
localeRules _ _            = []

langRules :: Some Dimension -> [Rule]
langRules (This AmountOfMoney) = []
langRules (This Distance) = Distance.rules
langRules (This Duration) = []
langRules (This Email) = []
langRules (This Numeral) = Numeral.rules
langRules (This Ordinal) = Ordinal.rules
langRules (This PhoneNumber) = []
langRules (This Quantity) = []
langRules (This RegexMatch) = []
langRules (This Temperature) = Temperature.rules
langRules (This Time) = Time.rules
langRules (This TimeGrain) = TimeGrain.rules
langRules (This Url) = []
langRules (This Volume) = []
