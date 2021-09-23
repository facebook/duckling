-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}


module Duckling.Rules.ZH
  ( defaultRules
  , langRules
  , localeRules
  ) where

import Duckling.Dimensions.Types
import Duckling.Locale
import Duckling.Types
import qualified Duckling.AmountOfMoney.ZH.Rules as AmountOfMoney
import qualified Duckling.Distance.ZH.Rules as Distance
import qualified Duckling.Duration.ZH.Rules as Duration
import qualified Duckling.Numeral.ZH.Rules as Numeral
import qualified Duckling.Ordinal.ZH.Rules as Ordinal
import qualified Duckling.Quantity.ZH.Rules as Quantity
import qualified Duckling.Temperature.ZH.Rules as Temperature
import qualified Duckling.Time.ZH.Rules as Time
import qualified Duckling.Time.ZH.CN.Rules as TimeCN
import qualified Duckling.Time.ZH.HK.Rules as TimeHK
import qualified Duckling.Time.ZH.MO.Rules as TimeMO
import qualified Duckling.Time.ZH.TW.Rules as TimeTW
import qualified Duckling.TimeGrain.ZH.Rules as TimeGrain
import qualified Duckling.Volume.ZH.Rules as Volume

defaultRules :: Seal Dimension -> [Rule]
defaultRules = langRules

localeRules :: Region -> Seal Dimension -> [Rule]
localeRules CN (Seal Time) = TimeCN.rules
localeRules HK (Seal Time) = TimeHK.rules
localeRules MO (Seal Time) = TimeMO.rules
localeRules TW (Seal Time) = TimeTW.rules
localeRules region (Seal (CustomDimension dim)) = dimLocaleRules region dim
localeRules _ _ = []

langRules :: Seal Dimension -> [Rule]
langRules (Seal AmountOfMoney) = AmountOfMoney.rules
langRules (Seal CreditCardNumber) = []
langRules (Seal Distance) = Distance.rules
langRules (Seal Duration) = Duration.rules
langRules (Seal Email) = []
langRules (Seal Numeral) = Numeral.rules
langRules (Seal Ordinal) = Ordinal.rules
langRules (Seal PhoneNumber) = []
langRules (Seal Quantity) = Quantity.rules
langRules (Seal RegexMatch) = []
langRules (Seal Temperature) = Temperature.rules
langRules (Seal Time) = Time.rules
langRules (Seal TimeGrain) = TimeGrain.rules
langRules (Seal Url) = []
langRules (Seal Volume) = Volume.rules
langRules (Seal (CustomDimension dim)) = dimLangRules ZH dim
