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

defaultRules :: Seal Dimension -> [Rule]
defaultRules dim@(Seal Time) = TimeNL.rulesBackwardCompatible ++ langRules dim
defaultRules dim = langRules dim

localeRules :: Region -> Seal Dimension -> [Rule]
localeRules region (Seal (CustomDimension dim)) = dimLocaleRules region dim
localeRules BE (Seal Time) = TimeBE.rules
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
langRules (Seal Temperature) = []
langRules (Seal Time) = Time.rules
langRules (Seal TimeGrain) = TimeGrain.rules
langRules (Seal Url) = []
langRules (Seal Volume) = Volume.rules
langRules (Seal (CustomDimension dim)) = dimLangRules NL dim
