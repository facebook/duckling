-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}


module Duckling.Rules.CA
  ( defaultRules
  , langRules
  , localeRules
  ) where


import Duckling.Dimensions.Types
import Duckling.Locale
import qualified Duckling.AmountOfMoney.CA.Rules as AmountOfMoney
import qualified Duckling.Distance.CA.Rules as Distance
import qualified Duckling.Duration.CA.Rules as Duration
import qualified Duckling.Numeral.CA.Rules as Numeral
import qualified Duckling.Ordinal.CA.Rules as Ordinal
import qualified Duckling.Temperature.CA.Rules as Temperature
import qualified Duckling.Time.CA.Rules as Time
import qualified Duckling.TimeGrain.CA.Rules as TimeGrain
import qualified Duckling.Volume.CA.Rules as Volume
import Duckling.Types

defaultRules :: Seal Dimension -> [Rule]
defaultRules = langRules

localeRules :: Region -> Seal Dimension -> [Rule]
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
langRules (Seal Quantity) = []
langRules (Seal RegexMatch) = []
langRules (Seal Temperature) = Temperature.rules
langRules (Seal Time) = Time.rules
langRules (Seal TimeGrain) = TimeGrain.rules
langRules (Seal Url) = []
langRules (Seal Volume) = Volume.rules
langRules (Seal (CustomDimension _)) = []
