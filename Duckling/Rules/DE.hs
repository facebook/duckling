-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
module Duckling.Rules.DE
  ( defaultRules
  , langRules
  , localeRules
  ) where

import Duckling.Dimensions.Types
import qualified Duckling.Distance.DE.Rules as Distance
import qualified Duckling.Duration.DE.Rules as Duration
import qualified Duckling.Email.DE.Rules as Email
import Duckling.Locale
import qualified Duckling.Numeral.DE.Rules as Numeral
import qualified Duckling.Ordinal.DE.Rules as Ordinal
import qualified Duckling.Time.DE.Rules as Time
import qualified Duckling.TimeGrain.DE.Rules as TimeGrain
import Duckling.Types
import qualified Duckling.Volume.DE.Rules as Volume

defaultRules :: Some Dimension -> [Rule]
defaultRules = langRules

localeRules :: Region -> Some Dimension -> [Rule]
localeRules region (This (CustomDimension dim)) = dimLocaleRules region dim
localeRules _ _ = []

langRules :: Some Dimension -> [Rule]
langRules (This AmountOfMoney) = []
langRules (This CreditCardNumber) = []
langRules (This Distance) = Distance.rules
langRules (This Duration) = Duration.rules
langRules (This Email) = Email.rules
langRules (This Numeral) = Numeral.rules
langRules (This Ordinal) = Ordinal.rules
langRules (This PhoneNumber) = []
langRules (This Quantity) = []
langRules (This RegexMatch) = []
langRules (This Temperature) = []
langRules (This Time) = Time.rules
langRules (This TimeGrain) = TimeGrain.rules
langRules (This Url) = []
langRules (This Volume) = Volume.rules
langRules (This (CustomDimension dim)) = dimLangRules DE dim
