-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}

module Duckling.Rules.NL
  ( defaultRules
  , langRules
  , localeRules
  ) where

import Prelude

import Duckling.Dimensions.Types
import Duckling.Locale
import Duckling.Types
import qualified Duckling.AmountOfMoney.NL.Rules as AmountOfMoney
import qualified Duckling.Distance.NL.Rules as Distance
import qualified Duckling.Duration.NL.Rules as Duration
import qualified Duckling.Numeral.NL.Rules as Numeral
import qualified Duckling.Ordinal.NL.Rules as Ordinal
import qualified Duckling.Quantity.NL.Rules as Quantity
import qualified Duckling.Time.NL.Rules as Time
import qualified Duckling.Time.NL.BE.Rules as TimeBE
import qualified Duckling.Time.NL.NL.Rules as TimeNL
import qualified Duckling.TimeGrain.NL.Rules as TimeGrain
import qualified Duckling.Volume.NL.Rules as Volume

defaultRules :: Some Dimension -> [Rule]
defaultRules dim@(This Time) = TimeNL.rulesBackwardCompatible ++ langRules dim
defaultRules dim = langRules dim

localeRules :: Region -> Some Dimension -> [Rule]
localeRules region (This (CustomDimension dim)) = dimLocaleRules region dim
localeRules BE (This Time) = TimeBE.rules
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
langRules (This Temperature) = []
langRules (This Time) = Time.rules
langRules (This TimeGrain) = TimeGrain.rules
langRules (This Url) = []
langRules (This Volume) = Volume.rules
langRules (This (CustomDimension dim)) = dimLangRules NL dim
