-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}


module Duckling.Rules.AR
  ( defaultRules
  , langRules
  , localeRules
  ) where

import Duckling.Dimensions.Types
import Duckling.Locale
import Duckling.Types
import qualified Duckling.AmountOfMoney.AR.Rules as AmountOfMoney
import qualified Duckling.Duration.AR.Rules as Duration
import qualified Duckling.Numeral.AR.Rules as Numeral
import qualified Duckling.Numeral.AR.EG.Rules as NumeralEG
import qualified Duckling.Ordinal.AR.Rules as Ordinal
import qualified Duckling.PhoneNumber.AR.Rules as PhoneNumber
import qualified Duckling.Quantity.AR.Rules as Quantity
import qualified Duckling.Temperature.AR.Rules as Temperature
import qualified Duckling.Time.AR.Rules as Time
import qualified Duckling.TimeGrain.AR.Rules as TimeGrain
import qualified Duckling.Volume.AR.Rules as Volume

defaultRules :: Seal Dimension -> [Rule]
defaultRules = langRules

localeRules :: Region -> Seal Dimension -> [Rule]
localeRules EG (Seal Numeral) = NumeralEG.rules
localeRules region (Seal (CustomDimension dim)) = dimLocaleRules region dim
localeRules _ _ = []

langRules :: Seal Dimension -> [Rule]
langRules (Seal AmountOfMoney) = AmountOfMoney.rules
langRules (Seal CreditCardNumber) = []
langRules (Seal Distance) = []
langRules (Seal Duration) = Duration.rules
langRules (Seal Email) = []
langRules (Seal Numeral) = Numeral.rules
langRules (Seal Ordinal) = Ordinal.rules
langRules (Seal PhoneNumber) = PhoneNumber.rules
langRules (Seal Quantity) = Quantity.rules
langRules (Seal RegexMatch) = []
langRules (Seal Temperature) = Temperature.rules
langRules (Seal Time) = Time.rules
langRules (Seal TimeGrain) = TimeGrain.rules
langRules (Seal Url) = []
langRules (Seal Volume) = Volume.rules
langRules (Seal (CustomDimension dim)) = dimLangRules AR dim
