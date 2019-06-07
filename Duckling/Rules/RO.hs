-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}


module Duckling.Rules.RO
  ( defaultRules
  , langRules
  , localeRules
  ) where

import Duckling.Dimensions.Types
import Duckling.Locale
import Duckling.Types
import qualified Duckling.AmountOfMoney.RO.Rules as AmountOfMoney
import qualified Duckling.Distance.RO.Rules as Distance
import qualified Duckling.Duration.RO.Rules as Duration
import qualified Duckling.Numeral.RO.Rules as Numeral
import qualified Duckling.Ordinal.RO.Rules as Ordinal
import qualified Duckling.Quantity.RO.Rules as Quantity
import qualified Duckling.Temperature.RO.Rules as Temperature
import qualified Duckling.Time.RO.Rules as Time
import qualified Duckling.TimeGrain.RO.Rules as TimeGrain
import qualified Duckling.Volume.RO.Rules as Volume

defaultRules :: Some Dimension -> [Rule]
defaultRules = langRules

localeRules :: Region -> Some Dimension -> [Rule]
localeRules region (This (CustomDimension dim)) = dimLocaleRules region dim
localeRules _ _ = []

langRules :: Some Dimension -> [Rule]
langRules (This AmountOfMoney) = AmountOfMoney.rules
langRules (This CreditCardNumber) = []
langRules (This Distance) = Distance.rules
langRules (This Duration) = Duration.rules
langRules (This Email) = []
langRules (This Numeral) = Numeral.rules
langRules (This Ordinal) = Ordinal.rules
langRules (This PhoneNumber) = []
langRules (This Quantity) = Quantity.rules
langRules (This RegexMatch) = []
langRules (This Temperature) = Temperature.rules
langRules (This Time) = Time.rules
langRules (This TimeGrain) = TimeGrain.rules
langRules (This Url) = []
langRules (This Volume) = Volume.rules
langRules (This (CustomDimension dim)) = dimLangRules RO dim
